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
  --let encoderCpr = 600*4 :: Uint16
  --
  -- 2400 pulses per mechanical revolution
  let encoderCpr = 600*4 :: Sint32
  -- 7 is the number of rotor poles (magnets)
  -- XXX: should be configurable
  let elecRadPerEnc = 7 * 2 * pi * (1/(safeCast encoderCpr)) :: IFloat

  monitor "encoder" $ do
    lastSample <- state "lastSample"

    encState <- stateInit "encState" (ival (0 :: Sint32))

    -- pll
    pllKp <- stateInit "pllkp" (ival (0 :: IFloat))
    pllKi <- stateInit "pllki" (ival (0 :: IFloat))
    pllPosition <- stateInit "pllPosition" (ival (0 :: IFloat))
    pllVelocity <- stateInit "pllVelocity" (ival (0 :: IFloat))

    --dbg
    elp <- state "elp"
    edelta <- state "delta"
    enewstate <- state "newstate"
    erlp <- state "rlp"
    edeltapos <- state "deltapos"

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
        enccount <- encoder_get_count
        encdir <- encoder_get_dir

        encs <- deref encState

         -- XXX: trickery from Sint32 -> Sint16
         -- we look at bottom 16 bits only as the hardware counter is also
         -- 32bit, this is exploited to get delta
        let delta :: Sint16
            delta = (twosComplementCast enccount) - (ivoryCast :: Sint32 -> Sint16) encs
        let newstate :: Sint32
            newstate = safeCast $ encs + (safeCast delta)

        store encState newstate

        let ph :: IFloat
            ph = safeCast (newstate .% encoderCpr) - encoderOffset

        let rotorPhase :: IFloat
            rotorPhase = (elecRadPerEnc * ph) .% (2*pi)

        comment "predict current position"
        pllpos <- deref pllPosition
        pllvel <- deref pllVelocity
        pllkp <- deref pllKp
        pllki <- deref pllKi

        let newpllpos = pllpos + (currentMeasPeriod cc) * pllvel

        comment "discrete phase detector"
        let deltaPos :: IFloat
            deltaPos = safeCast $ newstate - (castWith 0 $ floorF newpllpos)

        comment "feedback"
        store pllPosition $ newpllpos + (currentMeasPeriod cc) * pllkp * deltaPos
        store pllVelocity $ pllvel + (currentMeasPeriod cc) * pllki * deltaPos

        --dbg
        store enewstate newstate
        store elp ph
        store edelta delta
        store edeltapos deltaPos
        store erlp rotorPhase

        pllp <- deref pllPosition
        pllv <- deref pllVelocity

        sample <- local $ istruct
          [ count .= ival newstate
          , dir .= ival encdir
          , phase .= ival rotorPhase
          , pll_pos .= ival pllp
          , pll_vel .= ival pllv
          ]

        refCopy lastSample sample
        emit e (constRef sample)

  monitor "encoderSender" $ do
    encoderSender (snd encchan) uarto
