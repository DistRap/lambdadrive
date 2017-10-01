{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module ODrive.Tests.FOCCurrent where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Sched

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.SPI

import ODrive.ADC
import ODrive.Encoder
import ODrive.ExtInt
import ODrive.DRV8301
import ODrive.Platforms
import ODrive.LED
import ODrive.PWM
import ODrive.Types
import ODrive.Serialize
import ODrive.Calibration
import ODrive.Control.Modulation
import ODrive.Control.PID
import ODrive.Control.Transform
import LDrive.Ivory.Types.Adc
import LDrive.Ivory.Types.Encoder
import LDrive.Ivory.Types.AdcEncSample
import LDrive.Ivory.Types.Calibration
import LDrive.Ivory.Types.CalEnc
import LDrive.Ivory.Types.CurrentControl

app :: (e -> ClockConfig)
    -> (e -> ADCs)
    -> (e -> Enc)
    -> (e -> TestSPI)
    -> (e -> PWMOut)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestadcs totestenc totestspi totestpwm touart toleds = do
  odriveTowerDeps

  cc <- fmap tocc getEnv
  adcs  <- fmap totestadcs getEnv
  encm  <- fmap totestenc getEnv
  spi  <- fmap totestspi getEnv
  pwm  <- fmap totestpwm getEnv
  uart <- fmap touart getEnv
  leds <- fmap toleds getEnv

  let measPeriod = currentMeasPeriod cc

  blink (Milliseconds 1000) [redLED leds]
  blink (Milliseconds 666) [greenLED leds]

  Encoder{..} <- encoderTower encm

  ext <- extIntTower testExti

  let devices = [ drv8301M0
                , drv8301M1
                ]
  (sreq, sready) <- spiTower tocc devices (testSPIPins spi)

  (adc_chan, _adc_dc_chan, timingsIn) <- adcMultiTower adcs m0_dc_cal measPeriod (pwmTim pwm)
  PWM{..} <- pwmTower pwm

  (drvTask, drvReq) <- task "drv8301"
  (drvReady, drvFault) <- drvTower drvReq sready (SPIDeviceHandle 0)

  schedule "drvSchedule"
    [drvTask] sready sreq

  (calAdcEncIn, calDone) <- calibrationTower cc timingsIn

  (uarto, _istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200
  monitor "uart" mon
  cc_chan <- channel
  --div_adc <- rateDivider 1000 (adc_chan)
--  div_adc_dc <- rateDivider 1000 (adc_dc_chan)
  div_cc <- rateDivider 501 (snd cc_chan)
--  div_enc <- rateDivider 1000 (snd encchan)

  uartTasks <- sequence
    [ do
        (t, chan) <- task name
        monitor name $ f chan
        return t
    | (name, f) <-
--      [ ("adc", adcSender div_adc)
--      , ("dccal", dccalSender div_adc_dc)
      [ ("cc", currentControlSender div_cc)
--      , ("enc", encoderSender div_enc)
      ]
    ]

  schedule "uart" uartTasks systemInit uarto

  -- 2400 pulses per mechanical revolution
  let encoderCpr = 600*4 :: Sint32
      motorPoles = 7 :: Uint8
      bandwidth = 2000 :: IFloat
      kp = bandwidth * 2
      ki = (1/4 * kp ** 2) -- critically damped

  monitor "simplecontroller" $ do
    lastSample <- state "lastSample"
    encState <- state "encState"
    calibState <- state "calibState"
    control <- state "control"

    speedOffset <- stateInit "speedOffset" $ ival (0.0 :: IFloat)

    trig <- state "trig"
    trigFlip <- state "trigFlip"

    handler ext "ext" $ do
      callback $ const $ do
        store trig true
        t <- deref trig
        --assert (iNot t)
        tf <- deref trigFlip
        ifte_ (tf) (store trigFlip false >> ledOff (greenLED leds))
          (store trigFlip true >> ledOn (greenLED leds))
        speedOffset %= (+0.1)

        (control ~> q_in) %= (+0.1)

    handler systemInit "init" $ do
      callback $ const $ do

        encoderInitState motorPoles encoderCpr measPeriod kp ki encState

        -- check that we don't get problems with discrete time approximation
        assert ((currentMeasPeriod cc) * kp <? 1.0)

    pwmout <- stateInit "pwmout" $ iarray [ival 0, ival 0, ival 0]

    waitBusPhase <- stateInit "waitBusPhase" $ ival false
    calibPhase <- stateInit "calibPhase" $ ival false
    ready <- stateInit "ready" $ ival false

    handler drvReady "drvReady" $ do
      callback $ const $
        store waitBusPhase true
        --XXX: set to tim_period_clocks/2 on boot
        --pwm_set (constRef pwmout)

    handler drvFault "drvFault" $ do
      callback $ const $ store ready false

    handler calDone "calDone" $ do
      callback $ \x -> do
        refCopy calibState x

        di <- calibState ~> calEnc ~>* direction
        store (encState ~> enc_dir) di
        off <- calibState ~> calEnc ~>* offset
        store (encState ~> enc_offset) off

        store calibPhase false
        store ready true

        let qDes = 0.5
            pGain = 2.0
            iGain = 200.0
        store (control ~> q_in) qDes
        store (control ~> p_gain) pGain
        store (control ~> i_gain) iGain

    handler adc_chan "adc_chan" $ do
      timings <- emitter timingsIn 1
      calAdcEncE <- emitter calAdcEncIn 1
      ccE <- emitter (fst cc_chan) 1
      callback $ \adc -> do

        enc <- encoder_get encState

        refCopy (lastSample ~> adc_sample) adc
        refCopy (lastSample ~> enc_sample) enc
        isWaitBus <- deref waitBusPhase
        when isWaitBus $ do
          bus <- adc ~>* vbus
          when (bus >? 20.0) $ do
            store waitBusPhase false
            store calibPhase true

        isCal <- deref calibPhase
        when isCal $ do
          emit calAdcEncE $ constRef lastSample

        isready <- deref ready
        when isready $ do
          bus <- adc ~>* vbus
          pbI <- adc ~>* phase_b
          pcI <- adc ~>* phase_c

          theta <- enc ~>* phase
          pllVel <- enc ~>* pll_vel

          let (alpha', beta') = clarke pbI pcI
              (d', q') = park alpha' beta' theta
              vfactor = 1.0 / ((2.0 / 3.0) * bus)

          store (control ~> alpha) alpha'
          store (control ~> beta) beta'
          store (control ~> d) d'
          store (control ~> q) q'
          store (control ~> vfact) vfactor

          dIn <- control ~>* d_in
          qIn <- control ~>* q_in
          store (control ~> d_err) (dIn - d')
          store (control ~> q_err) (qIn - q')
          dErr <- control ~>* d_err
          qErr <- control ~>* q_err

          ciD <- control ~>* current_i_d
          ciQ <- control ~>* current_i_q
          ckp <- control ~>* p_gain
          cki <- control ~>* i_gain

          -- PI control
          let vD = ciD + dErr * ckp
              vQ = ciQ + qErr * ckp
          store (control ~> v_d) vD
          store (control ~> v_q) vQ

          let modD = vfactor * vD
              modQ = vfactor * vQ
              modS = 0.4 * (sqrt 3 / 2.0) * (1 / sqrt (modD ** 2 + modQ**2))
              decay = 0.99

          store (control ~> mod_d) modD
          store (control ~> mod_q) modQ
          store (control ~> mod_scale) modS

          ifte_ (modS <=? 1.0)
            (do
                store (control ~> scaled) true
                (control ~> mod_d) %= (*modS)
                (control ~> mod_q) %= (*modS)
                (control ~> current_i_d) %= (*decay)
                (control ~> current_i_q) %= (*decay)
                )
            (do
                store (control ~> scaled) false
                (control ~> current_i_d) %= (+(dErr * cki * measPeriod))
                (control ~> current_i_q) %= (+(qErr * cki * measPeriod))
                )

          modDout <- control ~>* mod_d
          modQout <- control ~>* mod_q

          store (control ~> ibus) $ d' * modDout + q' * modQout

          let (modAlpha, modBeta) = ipark modDout modQout theta

          current_modulation modAlpha modBeta pwmout

          store (control ~> mod_alpha) modAlpha
          store (control ~> mod_beta) modBeta

          emit timings (constRef pwmout)
          emit ccE (constRef control)
