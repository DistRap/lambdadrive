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
import Ivory.Tower

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART

import ODrive.Encoder
import ODrive.Platforms
import ODrive.LED
import ODrive.Types
import ODrive.Serialize

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

  (uarto, _istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200

  monitor "uart" mon

  Encoder{..} <- encoderTower enc

  periodic <- period (Milliseconds 10)

  encchan <- channel

  -- XXX measure offset
  let encoderOffset = 179 :: IFloat
  --let encoderCpr = 600*4 :: Uint16
  --
  -- 2400 pulses per mechanical revolution
  let encoderCpr = 600*4 :: Uint32
  -- 7 is the number of rotor poles (magnets)
  -- XXX: should be configurable
  let elecRadPerEnc = 7 * 2 * pi * (1/(safeCast encoderCpr)) :: IFloat

  monitor "encoder" $ do
    lastSample <- state "lastSample"

    encState <- stateInit "encState" (ival (0 :: Uint32))

    -- pll
    pllKp <- stateInit "pllkp" (ival (0 :: IFloat))
    pllKi <- stateInit "pllki" (ival (0 :: IFloat))

    elp <- state "elp"
    edelta <- state "delta"
    enewstate <- state "newstate"
    erlp <- state "rlp"

    handler systemInit "init" $ do
      callback $ const $ do
        let bandwidth = 2000 :: IFloat
        store pllKp (bandwidth * 2)
        -- critically damped
        kp <- deref pllKp
        store pllKi (1/4 * kp ** 2)

        -- check that we don't get problems with discrete time approximation
        assert ((currentMeasPeriod cc) * kp <? 1.0)

    handler periodic "encCvt" $ do
      e <- emitter (fst encchan) 1
      callback $ const $ do
        count <- encoder_get_count
        dir <- encoder_get_dir

        encs <- deref encState

        let delta = (safeCast count) - encs
        let newstate :: Uint32
            newstate = safeCast $ encs + delta

        store encState newstate

        let ph :: IFloat
            ph = safeCast (newstate .% encoderCpr) - encoderOffset

        let rotorPhase :: IFloat
            rotorPhase = (elecRadPerEnc * ph) .% (2*pi)

        --dbg
        store enewstate newstate
        store elp ph
        store edelta delta
        store erlp rotorPhase

        sample <- local $ istruct
          [ encoder_count .= ival newstate
          , encoder_dir .= ival dir
          , encoder_phase .= ival rotorPhase]

        refCopy lastSample sample
        emit e (constRef sample)

  monitor "encoderSender" $ do
    encoderSender (snd encchan) uarto
