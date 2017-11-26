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

module LDrive.Tests.Spin where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Sched

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.SPI

import LDrive.ADC
import LDrive.Encoder
import LDrive.DRV8301
import LDrive.Platforms
import LDrive.LED
import Ivory.Tower.Drivers.PWM.ATIM
import LDrive.Types
import LDrive.Serialize
import LDrive.Control.Modulation
import LDrive.Control.SVM

app :: (e -> ClockConfig)
    -> (e -> ADCs)
    -> (e -> Enc)
    -> (e -> TestSPI)
    -> (e -> PWMTimer)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestadcs totestenc totestspi totestpwm touart toleds = do
  ldriveTowerDeps

  cc <- fmap tocc getEnv
  adcs  <- fmap totestadcs getEnv
  enc  <- fmap totestenc getEnv
  spi  <- fmap totestspi getEnv
  pwm  <- fmap totestpwm getEnv
  uart <- fmap touart getEnv
  leds <- fmap toleds getEnv

  let measPeriod = currentMeasPeriod cc

  blink (Milliseconds 1000) [redLED leds]
  blink (Milliseconds 666) [greenLED leds]

  (uarto, _istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200
  monitor "uart" mon

  Encoder{..} <- encoderTower enc

  let devices = [ drv8301M0
                , drv8301M1
                ]
  (sreq, sready) <- spiTower tocc devices (testSPIPins spi)

  (adc_chan, adc_dc_chan, _timings) <- adcMultiTower adcs m0_dc_cal measPeriod (pwmTim pwm)
  PWM{..} <- pwmTower pwm

  periodic <- period (Milliseconds 11)

  encchan <- channel

  (drvTask, drvReq) <- task "drv8301"
  (drvReady, drvFault) <- drvTower drvReq sready (SPIDeviceHandle 0)

  schedule "drvSchedule"
    [drvTask] sready sreq

  -- 2400 pulses per mechanical revolution
  let encoderCpr = 600*4 :: Sint32
      motorPoles = 7 :: Uint8
      bandwidth = 2000 :: IFloat
      kp = bandwidth * 2
      ki = (1/4 * kp ** 2) -- critically damped

  monitor "encoder" $ do
    lastSample <- state "lastSample"
    encState <- state "encState"

    handler systemInit "init" $ do
      callback $ const $ do

        encoderInitState motorPoles encoderCpr (currentMeasPeriod cc) kp ki encState

        -- check that we don't get problems with discrete time approximation
        assert ((currentMeasPeriod cc) * kp <? 1.0)

    handler periodic "encCvt" $ do
      e <- emitter (fst encchan) 1
      callback $ const $ do

        sample <- encoder_get encState

        refCopy lastSample sample
        emit e sample

  svm_chan <- channel
  div_adc <- rateDivider 1110 (adc_chan)
  div_adc_dc <- rateDivider 1110 (adc_dc_chan)
  div_svm <- rateDivider 10 (snd svm_chan)
  div_enc <- rateDivider 10 (snd encchan)

  uartTasks <- sequence
    [ do
        (t, chan) <- task name
        monitor name $ f chan
        return t
    | (name, f) <-
      [ ("adc", adcSender div_adc)
      , ("dccal", dccalSender div_adc_dc)
      , ("svm", svmSender div_svm)
      , ("enc", encoderSender div_enc)
      ]
    ]

  schedule "uart" uartTasks systemInit uarto

  monitor "simplecontroller" $ do
    pwmout <- stateInit "pwmout" $ iarray [ival 0, ival 0, ival 0]
    cphase <- stateInit "cphase" $ ival 0
    ready <- stateInit "ready" $ ival false

    handler drvReady "drvReady" $ do
      callback $ const $
        store ready true
        --XXX: set to tim_period_clocks/2 on boot
        --pwm_set (constRef pwmout)

    handler drvFault "drvFault" $ do
      callback $ const $ store ready false

    handler periodic "period" $ do
      svm_e <- emitter (fst svm_chan) 1
      callback $ const $ do

        isready <- deref ready
        when isready $ do

          ph <- deref cphase

          let alpha = 0.5 * cos ph
          let beta = 0.5 * sin ph

          voltage_modulation 24 alpha beta pwmout

          svmout <- svm alpha beta
          emit svm_e $ svmout

          pwm_set (constRef pwmout)

          store cphase ((ph + 0.1) .% (2*pi))
