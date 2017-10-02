{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LDrive.Tests.SPI where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.Sched

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Peripheral.SPI

import LDrive.Platforms
import LDrive.LED
import LDrive.DRV8301
import LDrive.Types
import LDrive.Utils

app :: (e -> ClockConfig)
    -> (e -> TestSPI)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestspi touart toleds = do
  ldriveTowerDeps

  spi  <- fmap totestspi getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv
  blink (Milliseconds 1000) [redLED leds]

  (buffered_ostream, _istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200

  monitor "uart" mon
  -- UART buffer transmits in buffers. We want to transmit byte-by-byte and let
  -- this monitor manage periodically flushing a buffer.
  ostream <- uartUnbuffer (buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))

  let devices = [ drv8301M0
                , drv8301M1
                ]

  (sreq, sready) <- spiTower tocc devices (testSPIPins spi)

  initdone_sready <- channel
  monitor "drv_enable" $ do
    -- this just re-emits sready on initdone_sready, we could just use sready as well..
    handler sready "init" $ do
      e <- emitter (fst initdone_sready) 1
      callback $ \t -> do
        emit e t

  (drvTask0, drvReq0) <- task "drv8301_m0"
  drvTower drvReq0 (snd initdone_sready) (SPIDeviceHandle 0)

  (drvTask1, drvReq1) <- task "drv8301_m1"
  drvTower drvReq1 (snd initdone_sready) (SPIDeviceHandle 1)

  schedule ("drv")
    [drvTask0, drvTask1] sready sreq

  periodic <- period (Milliseconds 500)

  monitor "simplecontroller" $ do
    handler periodic "periodic" $ do
      o <- emitter ostream 64
      callback $ \_ -> do
        puts o "q"
