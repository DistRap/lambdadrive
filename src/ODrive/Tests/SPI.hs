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
import Ivory.Tower.HAL.Bus.Sched
import qualified Ivory.Tower.HAL.Bus.SchedAsync as Async

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Peripheral.CAN.Filter
import Ivory.BSP.STM32.Peripheral.SPI -- as SPI

import ODrive.Platforms
import ODrive.LED
import ODrive.DRV8301
import ODrive.Types
import ODrive.Utils

[ivory|
  bitdata MSG :: Bits 16 = drv8301msg
   { d_rw :: DrvRW
   , d_addr :: DrvAddr
   , d_data :: Bits 11
   }
|]

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

r_s1 :: MSG
r_s1 = m drv_read drv_status1

r_s2 :: MSG
r_s2 = m drv_read drv_status2

-- for odrive this is C1 $11 = 0x1550
-- and C2 $12 = 0x1808, status1 0x0, status2 0x801

m :: DrvRW -> DrvAddr -> MSG
m rw addr = fromRep $ withBits 0 $ do
  setField d_rw rw
  setField d_addr addr

m' :: DrvRW -> DrvAddr -> Bits 11 -> MSG
m' rw addr c = fromRep $ withBits 0 $ do
  setField d_rw rw
  setField d_addr addr
  setField d_data c

spi_req :: (GetAlloc eff ~ 'Scope s)
        => SPIDeviceHandle
        -> MSG
        -> Ivory eff (ConstRef ('Stack s) ('Struct "spi_transaction_request"))
spi_req dev msg = fmap constRef $ local $ istruct
              [ tx_device .= ival dev -- (SPIDeviceHandle 0)
              , tx_buf    .= iarray [h msg, l msg]
              , tx_len    .= ival 2
              ]
  where l x = ival $ bitCast $ toRep x
        h x = ival $ bitCast $ (toRep x) `iShiftR` 8


drvTower :: (IvoryZero init, IvoryArea init) =>
            BackpressureTransmit
              ('Struct "spi_transaction_request")
              ('Struct "spi_transaction_result")
            -> ChanOutput init
            -> ChanInput ('Stored Uint8)
            -> SPIDeviceHandle
            -> Tower e ()
drvTower (BackpressureTransmit req_c res_c) init_chan ostream dev = do
  periodic <- period (Milliseconds 500)

  monitor "drv8301mon" $ do
    write <- stateInit "write" (ival true)
    ready <- stateInit "ready" (ival false)
    retries <- stateInit "retries" (ival (0 :: Uint8))
    rounds <- stateInit "rounds" (ival (0 :: Uint8))
    handler systemInit "init" $ do
      callback $ const $ do
        -- enable en_gate pin connected to both DRVs
        pinOut drv8301_en_gate
        pinHigh drv8301_en_gate

    coroutineHandler init_chan res_c "drv8301" $ do
      req_e <- emitter req_c 1
      o <- emitter ostream 64
      return $ CoroutineBody $ \ yield -> do
        comment "drv coro start"
        let (SPIDeviceHandle x) = dev
        puts o "start"
        putc o (48 + x)
        -- rpc is nice, you emit request and yield
        -- while your request is processed
        --let rpc req = req >>= emit req_e >> yield
        -- but IT RESPONDS IN THE NEXT CYCLE!!!
        -- so we emit twice and return the second reply
        --let rpc req = req >>= emit req_e >> yield >> req >>= emit req_e >> yield
        -- which is ~equal to
        let rpc req = do
              x <- req
              emit req_e x
              yield -- previous command response
              emit req_e x
              yield -- our response

        let putResp o r = do
              arrayMap $ \ix -> do
                when (fromIx ix <? 2) $ do
                  val <- deref ((r ~> rx_buf) ! ix)
                  --putc o (48 + (castWith 0 $ fromIx $ ix))
                  comment "drv put"
                  putc o val

        comment "Initialization routine"
        comment "Loop until register changes responding to our write or we reach retries"
        forever $ do
          _ <- rpc (spi_req dev w_c1)
          r <- rpc (spi_req dev r_c1)

          rval <- deref ((r ~> rx_buf) ! 0)
          --puts o "r"
          --putc o rval

          -- FIXME: should compare to w_c1 data, I know it's 0x1550 so I can cheat for now
          when (rval ==? 0x15) breakOut

          rs <- deref retries
          store retries (rs + 1)
          when (rs >? 100) retVoid

        store ready true

        comment "drv coro forever"
        forever $ do
          comment "wait for response triggered by periodic request"
          r0 <- yield
          return ()
          comment "debug helpers"
          r <- deref rounds
          ifte_ (r ==? 200) (puts o "-" >> store rounds (r+1)) (store rounds (r+1))
          putc o (48 + x)
          puts o "i"
          putResp o r0

    handler periodic "periodic" $ do
      req_e <- emitter req_c 1
      callback $ \_ -> do
        isReady <- deref ready
        -- FIXME: read both status registers
        when isReady $ (spi_req dev r_s1) >>= emit req_e

app :: (e -> ClockConfig)
    -> (e -> TestSPI)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestspi touart toleds = do
  towerDepends odriveTypes
  towerModule  odriveTypes

  spi  <- fmap totestspi getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv

  (buffered_ostream, istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200

  monitor "dma" mon
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
  drvTower drvReq0 (snd initdone_sready) ostream (SPIDeviceHandle 0)

  (drvTask1, drvReq1) <- task "drv8301_m1"
  drvTower drvReq1 (snd initdone_sready) ostream (SPIDeviceHandle 1)

  schedule (spiName $ testSPIPeriph spi)
    [drvTask0, drvTask1] sready sreq

  periodic <- period (Milliseconds 500)

  monitor "simplecontroller" $ do
    write <- stateInit "write" (ival true)
    handler systemInit "init" $ do
      callback $ const $ do
        ledSetup $ redLED leds
        ledSetup $ blueLED leds

    handler periodic "periodic" $ do
      o <- emitter ostream 64
      callback $ \_ -> do
        puts o "q"
