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

module ODrive.Tests.Calib where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Sched

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.SPI

import ODrive.ADC
import ODrive.Encoder
import ODrive.DRV8301
import ODrive.Platforms
import ODrive.LED
import ODrive.PWM
import ODrive.Types
import ODrive.Calibration
import ODrive.Control.Modulation
import ODrive.Ivory.Types.Adc
import ODrive.Ivory.Types.AdcEncSample
import ODrive.Ivory.Types.Calibration
import ODrive.Ivory.Types.CalEnc

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
  _uart <- fmap touart getEnv
  leds <- fmap toleds getEnv

  let measPeriod = currentMeasPeriod cc

  blink (Milliseconds 1000) [redLED leds]
  blink (Milliseconds 666) [greenLED leds]

  Encoder{..} <- encoderTower encm

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

        d <- calibState ~> calEnc ~>* direction
        store (encState ~> enc_dir) d
        off <- calibState ~> calEnc ~>* offset
        store (encState ~> enc_offset) off

        store calibPhase false
        store ready true

    handler adc_chan "adc_chan" $ do
      timings <- emitter timingsIn 1
      calAdcEncE <- emitter calAdcEncIn 1
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

          bus <- deref (adc ~> vbus)
          voltage_modulation bus 0.0 0.0 pwmout

          emit timings (constRef pwmout)
