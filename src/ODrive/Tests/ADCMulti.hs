{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ODrive.Tests.ADCMulti where

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Sched

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART
import ODrive.ADC
import ODrive.Platforms
import ODrive.Types
import ODrive.LED
import ODrive.PWM
import ODrive.Utils
import ODrive.Serialize

app :: (e -> ClockConfig)
    -> (e -> ADCs)
    -> (e -> PWMOut)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc  totestadcs totestpwm touart toleds = do
  odriveTowerDeps

  cc <- fmap tocc getEnv
  adcs <- fmap totestadcs getEnv
  pwm  <- fmap totestpwm getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv

  blink (Milliseconds 1000) [redLED leds]

  (uarto, _uarti, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200
  monitor "uart" mon

  (adc_chan, adc_dc_chan, _timings) <- adcMultiTower adcs m0_dc_cal (currentMeasPeriod cc) (pwmTim pwm)
  -- pwm must go after adc as it starts to trigger all adcs simultaneously
  _ <- pwmTower pwm

  debugTower gpio1 adc_chan
  debugTower gpio1 adc_dc_chan

  div_adc <- rateDivider 100 (adc_chan)
  div_adc_dc <- rateDivider 100 (adc_dc_chan)

  uartTasks <- sequence
    [ do
        (t, chan) <- task name
        monitor name $ f chan
        return t
    | (name, f) <-
      [ ("adc", adcSender div_adc)
      , ("dccal", dccalSender div_adc_dc)
      ]
    ]

  schedule "uart" uartTasks systemInit uarto
