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

module ODrive.Tests.Encoder where

import Ivory.Language
import Ivory.Language.Cast
import Ivory.Tower

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART

import ODrive.Encoder
import ODrive.Platforms
import ODrive.LED
import ODrive.Types
import ODrive.Serialize
import ODrive.Ivory.Types.Encoder

app :: (e -> ClockConfig)
    -> (e -> Enc)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestenc touart toleds = do
  odriveTowerDeps

  cc <- fmap tocc getEnv
  enc  <- fmap totestenc getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv

  blink (Milliseconds 1000) [redLED leds]
  blink (Milliseconds 666) [greenLED leds]

  (uarto, _istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200

  monitor "uart" mon

  Encoder{..} <- encoderTower enc

  periodic <- period (Milliseconds 10)

  encchan <- channel

  -- XXX measure offset
  let encoderOffset = 179 :: IFloat
  -- 2400 pulses per mechanical revolution
  let encoderCpr = 600*4 :: Sint32

  monitor "encoder" $ do
    lastSample <- state "lastSample"

    encState <- state "encState"

    handler systemInit "init" $ do
      callback $ const $ do
        let bandwidth = 2000 :: IFloat
            kp = bandwidth * 2
            ki = (1/4 * kp ** 2) -- critically damped

        store (encState ~> enc_pll_kp) kp
        store (encState ~> enc_pll_ki) ki
        store (encState ~> enc_pll_period) $ currentMeasPeriod cc

        -- XXX: make configurable
        store (encState ~> enc_poles) 7 -- 7 is the number of rotor poles (magnets)
        store (encState ~> enc_cpr) encoderCpr
        store (encState ~> enc_offset) encoderOffset

        -- check that we don't get problems with discrete time approximation
        assert ((currentMeasPeriod cc) * kp <? 1.0)

    handler periodic "encCvt" $ do
      e <- emitter (fst encchan) 1
      callback $ const $ do

        sample <- encoder_get encState

        refCopy lastSample sample
        emit e sample

  monitor "encoderSender" $ do
    encoderSender (snd encchan) uarto
