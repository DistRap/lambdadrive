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
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART

import ODrive.Encoder
import ODrive.Platforms
import ODrive.LED
import ODrive.Types
import ODrive.Utils

app :: (e -> ClockConfig)
    -> (e -> Enc)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestenc touart toleds = do
  odriveTowerDeps

  enc  <- fmap totestenc getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv

  (buffered_ostream, _istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200

  monitor "uart" mon
  -- UART buffer transmits in buffers. We want to transmit byte-by-byte and let
  -- this monitor manage periodically flushing a buffer.
  ostream <- uartUnbuffer (buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))

  Encoder{..} <- encoderTower enc

  periodic <- period (Milliseconds 500)

  monitor "simplecontroller" $ do
    x <- state "x"
    handler systemInit "init" $ do
      callback $ const $ do
        ledSetup $ redLED leds
        ledSetup $ blueLED leds

    handler periodic "periodic" $ do
      o <- emitter ostream 64
      callback $ \_ -> do
        val <- encoder_get_count
        store x val
        puts o "q"
