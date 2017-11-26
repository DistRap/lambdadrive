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

module LDrive.Tests.FOCVoltage where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Sched
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface
import Ivory.HW.Module

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter

import LDrive.ADC
import LDrive.Encoder
import LDrive.ExtInt
import LDrive.DRV8301
import LDrive.Platforms
import LDrive.LED
import Ivory.Tower.Drivers.PWM.ATIM
import LDrive.Types
import LDrive.Calibration
import LDrive.Control.Modulation
import LDrive.Ivory.Types.Adc
import LDrive.Ivory.Types.Encoder
import LDrive.Ivory.Types.AdcEncSample
import LDrive.Ivory.Types.Calibration
import LDrive.Ivory.Types.CalEnc

import CANOpen.Tower
import CANOpen.Tower.Attr
import CANOpen.Tower.Types
import CANOpen.Tower.Interface.Cia402.Dict

app :: (e -> ClockConfig)
    -> (e -> ADCs)
    -> (e -> TestCAN)
    -> (e -> Enc)
    -> (e -> TestSPI)
    -> (e -> PWMTimer)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestadcs totestcan totestenc totestspi totestpwm touart toleds = do
  ldriveTowerDeps

  cc <- fmap tocc getEnv
  adcs  <- fmap totestadcs getEnv
  can  <- fmap totestcan getEnv
  encm  <- fmap totestenc getEnv
  spi  <- fmap totestspi getEnv
  pwm  <- fmap totestpwm getEnv
  _uart <- fmap touart getEnv
  leds <- fmap toleds getEnv

  let measPeriod = currentMeasPeriod cc

  blink (Milliseconds 1000) [redLED leds]
  blink (Milliseconds 666) [greenLED leds]

  (res, req, _, _) <- canTower tocc (testCAN can) 1000000 (testCANRX can) (testCANTX can)

  Encoder{..} <- encoderTower encm

  attrs@Cia402Attrs{..} <- towerCia402Attrs initCia402Attrs
  od@ObjDict{..} <- objDictTower attrs
  canopenTower res req (canOpenLEDs leds) od

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

    speedOffset <- stateInit "speedOffset" $ ival (0.0 :: IFloat)

    drvfault <- state "drvfault"


    received <- stateInit "received" (ival (0 :: Uint32))
    command <- stateInit "command" (ival (0 :: Uint8))

    -- we take targetVelocity from CANOpen dictionary divided by 10 
    -- as a very simple control mechanism
    attrHandler targetVelocity $ callbackV $ \vel -> do
        store speedOffset (safeCast ((signCast :: Sint32 -> Uint32) vel) / 10)

    handler systemInit "init" $ do
      callback $ const $ do

        encoderInitState motorPoles encoderCpr measPeriod kp ki encState

        -- check that we don't get problems with discrete time approximation
        assert ((currentMeasPeriod cc) * kp <? 1.0)

        -- empty can filter
        let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
        canFilterInit (testCANFilters can)
                      [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID]
                      []

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
      callback $ \fault -> do
        refCopy drvfault fault
        store ready false

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
      velE <- attrEmitter velocityActual
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
          ph <- deref (enc ~> phase)

          vel <- enc ~>* pll_vel
          emitV velE $ castDefault vel

          so <- deref speedOffset

          let c = cos ph
              s = sin ph
              vD = 0.0
              vQ = 0.4 + so
              vA = c * vD - s * vQ
              vB = c * vQ + s * vD

          voltage_modulation bus vA vB pwmout

          emit timings (constRef pwmout)

   where canOpenLEDs leds =
          CANOpenLEDs
            { leds_init = ledSetup (greenLED leds) >> ledSetup (redLED leds)
            , leds_module = hw_moduledef
            , leds_err_on = ledOn $ redLED leds
            , leds_err_off = ledOff $ redLED leds
            , leds_run_on = ledOn $ greenLED leds
            , leds_run_off = ledOff $ greenLED leds
            }
