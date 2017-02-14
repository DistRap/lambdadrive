{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ODrive.Tests.SPI where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Peripheral.CAN.Filter
--import Ivory.BSP.STM32.Peripheral.SPI -- as SPI

import ODrive.Platforms
import ODrive.LED
import BSP.Tests.UART.Buffer
import BSP.Tests.UART.Types

import ODrive.Tests.DRV
import ODrive.Tests.DRVRegs


[ivory|
  bitdata MSG :: Bits 16 = drv8301msg
   { d_rw :: DrvRW
   , d_addr :: DrvAddr
   , d_data :: Bits 11
   }

-- struct drv_msg { dmsg :: Stored Control1 }
|]

--drvTestTypes :: Module
--drvTestTypes = package "drvTypes" $ do
--  defStruct (Proxy :: Proxy "drv_msg")

w_c1 :: MSG
w_c1 = m' drv_write drv_control1 $
  repToBits $ withBits 0 $ do
    setField drv_ocmode drv_ocmode_latch_shutdown
    setField drv_ocadj drv_ocadj_0p730

w_c2 :: MSG
w_c2 = m' drv_write drv_control2 $
  repToBits $ withBits 0 $ do
    setField drv_gain drv_gain_40

r_c1 :: MSG
r_c1 = m drv_read drv_control1

r_c2 :: MSG
r_c2 = m drv_read drv_control2

m :: DrvRW -> DrvAddr -> MSG
m rw addr = fromRep $ withBits 0 $ do
  setField d_rw rw
  setField d_addr addr

m' :: DrvRW -> DrvAddr -> Bits 11 -> MSG
m' rw addr c = fromRep $ withBits 0 $ do
  setField d_rw rw
  setField d_addr addr
  setField d_data (c)

-- for odrive this is C1 $11 = 0x1550
-- and C2 $12 = 0x1808, status1 0x0, status2 0x801

spi_req h l = istruct
              [ tx_device .= ival (SPIDeviceHandle 0)
              , tx_buf    .= iarray [ival h, ival l]
              , tx_len    .= ival 2
              ]


puts :: (GetAlloc eff ~ 'Scope cs)
     => Emitter ('Stored Uint8) -> String -> Ivory eff ()
puts e str = mapM_ (\c -> putc e (fromIntegral (ord c))) str

putc :: (GetAlloc eff ~ 'Scope cs)
     => Emitter ('Stored Uint8) -> Uint8 -> Ivory eff ()
putc = emitV

app :: (e -> ClockConfig)
    -> (e -> TestSPI)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestspi touart toleds = do
  towerDepends uartTestTypes
  towerModule  uartTestTypes
--  towerDepends drvTestTypes
--  towerModule  drvTestTypes

  spi  <- fmap totestspi getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv

  (buffered_ostream, istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200

  monitor "dma" mon
  -- UART buffer transmits in buffers. We want to transmit byte-by-byte and let
  -- this monitor manage periodically flushing a buffer.
  ostream <- uartUnbuffer (buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))

  let devices = [drv8301]
  --(sreq, sready) <- spiTower tocc devices (testSPIPins spi)
  (BackpressureTransmit req res, _ready) <- spiTower tocc devices (testSPIPins spi)

  periodic <- period (Milliseconds 500)

  monitor "simplecontroller" $ do
    write <- stateInit "write" (ival true)
    handler systemInit "init" $ do
      callback $ const $ do
        ledSetup $ redLED leds
        ledSetup $ blueLED leds

        pinOut drv8301_en_gate
        pinHigh drv8301_en_gate

        pinOut m1_ncs
        pinHigh m1_ncs

    handler periodic "periodic" $ do
      req_e <- emitter req 1
      o <- emitter ostream 64

      let spiEmit ::(GetAlloc eff ~ 'Scope cs) => Uint8 -> Uint8 -> Ivory eff ()
          spiEmit h l = do
            r <- local $ spi_req h l
            emit req_e (constRef r)
      callback $ \_ -> do
        --x <- local $ ival 0
        --let x = fromRep $ withBits 0 $ do
        --            setBit drv_rw
        --
        let x = w_c2
        let (l, h) = (bitCast $ toRep x, bitCast $ (toRep x) `iShiftR` 8)

        let y = r_c2
        let (l2, h2) = (bitCast $ toRep y, bitCast $ (toRep y) `iShiftR` 8)

        --puts o "\n\rsnd\n\r"

        wr <- deref write
        --ifte_ wr (puts o "w" >> putc o h >> putc o l) (puts o "r" >> putc o h2 >> putc o l2)
        --ifte_ wr (puts o "w") (puts o "r")
        ifte_ wr (store write false >> spiEmit h l) (store write true >> spiEmit h2 l2)

    handler res "spiResult" $ do
      o <- emitter ostream 64
      callback $ \r -> do
        --code <- deref (r ~> resultcode)
        --len <- deref (r ~> rx_idx)
        --puts o "i"

        --putc o (48 + (castWith 0 $ fromIx len))
        arrayMap $ \ix -> do
          when (fromIx ix <? 2) $ do
            val <- deref ((r ~> rx_buf) ! ix)
            --putc o (48 + (castWith 0 $ fromIx $ ix))
            putc o val
        --puts o "\n\r/rcv\n\r"

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral $ ord c)

uartTestTypes :: Module
uartTestTypes = package "uartTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)
