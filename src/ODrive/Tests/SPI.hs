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
  setField d_data c

-- for odrive this is C1 $11 = 0x1550
-- and C2 $12 = 0x1808, status1 0x0, status2 0x801

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
--    handler systemInit "init" $ do
--      callback $ const $ do
--        pinOut drv8301_en_gate
--        pinHigh drv8301_en_gate
--
--        pinOut m1_ncs
--        pinHigh m1_ncs


    coroutineHandler init_chan res_c "drv8301" $ do
      req_e <- emitter req_c 1
      o <- emitter ostream 64
      return $ CoroutineBody $ \ yield -> do
        comment "drv coro start"
        puts o "start"
        let (SPIDeviceHandle x) = dev
        putc o (48 + x)
        --let rpc req = req >>= emit req_e >> yield
        -- IT RESPONDS IN NEXT CYCLE!!!
        -- so we emit twice
        --let rpc req = req >>= emit req_e >> yield >> req >>= emit req_e >> yield
        -- ~equal to
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

        comment "Loop until register changes or we reach retries"
        forever $ do
          -- IT RESPONDS IN NEXT CYCLE!!!
          rw <- rpc (spi_req dev w_c1)
          rr <- rpc (spi_req dev r_c1)
          --r <- yield
          wval <- deref ((rw ~> rx_buf) ! 0)
          putc o (48 + x)
          puts o "w"
          putc o wval

          rval <- deref ((rr ~> rx_buf) ! 0)
          puts o "r"
          putc o rval

          -- FIXME: should compare to w_c1 data
          when (rval ==? 0x15) breakOut

          rs <- deref retries
          store retries (rs + 1)
          when (rs >? 100) retVoid

        store ready true

        comment "drv coro forever"
        forever $ do
          --c1w <- (spi_req dev w_c1)
          --emit req_e c1w
          --r <- yield
          --puts o "y"
          --putResp o r
          --puts o "w"
          comment "drv coro r"
          --puts o "r"
          r0 <- yield -- rpc (spi_req dev r_c1)
          comment "drv coro resp"
          r <- deref rounds
          ifte_ (r ==? 200) (puts o "-" >> store rounds (r+1)) (store rounds (r+1))
          putc o (48 + x)
          puts o "i"
          putResp o r0

        --forever $ do
        --  rx <- rpc (spi_req dev r_c1)
        --  --comment "drv coro resp"
        --  putc o 'i'
          --putc o (48 + x)
          --putResp o rx
        --r1 <- rpc (spi_req dev r_c1)
        --comment "drv coro resp"
        --puts o "1"
        --putResp o r1
        --r <- rpc (spi_req dev r_c1)
        --comment "drv coro resp"
        --puts o "2"
        --arrayMap $ \ix -> do
        --  when (fromIx ix <? 2) $ do
        --    val <- deref ((r ~> rx_buf) ! ix)
        --    --putc o (48 + (castWith 0 $ fromIx $ ix))
        --    putc o val
        --putResp o r2

--        puts o "a"
--        b <- rpc (spi_req dev w_c1)
--        comment "drv coro r"
--        puts o "b"
--        putResp o b
--        r1 <- rpc (spi_req dev r_c1)
--        comment "drv coro resp"
--        puts o "c"
--        putResp o r1
        --retVoid

    handler periodic "periodic" $ do
      req_e <- emitter req_c 1
      callback $ \_ -> do
        isReady <- deref ready
        when isReady $ (spi_req dev r_c1) >>= emit req_e

--      o <- emitter ostream 64
--
----      let spiEmit ::(GetAlloc eff ~ 'Scope cs) => Uint8 -> Uint8 -> Ivory eff ()
----          spiEmit h l = do
----            r <- local $ spi_req h l
----            emit req_e (constRef r)
--      callback $ \_ -> do
--        let rpc req = req >>= emit req_e -- >> yield
--        let x = w_c2
--        let (l, h) = (bitCast $ toRep x, bitCast $ (toRep x) `iShiftR` 8)
--
--        let y = r_c2
--        --let (l2, h2) = (bitCast $ toRep y, bitCast $ (toRep y) `iShiftR` 8)
--
--        --puts o "\n\rsnd\n\r"
--
--        wr <- deref write
--        --ifte_ wr (puts o "w" >> putc o h >> putc o l) (puts o "r" >> putc o h2 >> putc o l2)
--        --ifte_ wr (puts o "w") (puts o "r")
--        ifte_ wr (store write false >> rpc (spi_req dev x)) (store write true >> rpc (spi_req dev y))
--
--    handler res_c "spiResult" $ do
--      o <- emitter ostream 64
--      callback $ \r -> do
--        --code <- deref (r ~> resultcode)
--        --len <- deref (r ~> rx_idx)
--        --puts o "i"
--
--        --putc o (48 + (castWith 0 $ fromIx len))
--        arrayMap $ \ix -> do
--          when (fromIx ix <? 2) $ do
--            val <- deref ((r ~> rx_buf) ! ix)
--            --putc o (48 + (castWith 0 $ fromIx $ ix))
--            putc o val
--        --puts o "\n\r/rcv\n\r"

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

  let devices = [ drv8301M0
                , drv8301M1
                ]

  (sreq, sready) <- spiTower tocc devices (testSPIPins spi)
  --(BackpressureTransmit req res, sready) <- spiTower tocc devices (testSPIPins spi)

  initdone_sready <- channel
  monitor "drv_enable" $ do
    handler sready "init" $ do
      e <- emitter (fst initdone_sready) 1
      callback $ \t -> do
        -- enable en_gate pin connected to both DRVs
        pinOut drv8301_en_gate
        pinHigh drv8301_en_gate

        emit e t

  --drv_s <- channel

  --drvTower sreq (snd initdone_sready) ostream (SPIDeviceHandle 0)

  (drvTask0, drvReq0) <- task "drv8301_m0"
  drvTower drvReq0 (snd initdone_sready) ostream (SPIDeviceHandle 0)

  (drvTask1, drvReq1) <- task "drv8301_m1"
  drvTower drvReq1 (snd initdone_sready) ostream (SPIDeviceHandle 1)

  schedule (spiName $ testSPIPeriph spi)
---    [drvTask0] sready sreq
    [drvTask0, drvTask1] sready sreq

  periodic <- period (Milliseconds 500)

  monitor "simplecontroller" $ do
    write <- stateInit "write" (ival true)
    handler systemInit "init" $ do
      callback $ const $ do
        ledSetup $ redLED leds
        ledSetup $ blueLED leds

   --     pinOut m1_nCS
   --     pinHigh m1_nCS

    handler periodic "periodic" $ do
--      req_e <- emitter req 1
      o <- emitter ostream 64
      callback $ \_ -> do
        puts o "q"
--
--      let spiEmit ::(GetAlloc eff ~ 'Scope cs) => Uint8 -> Uint8 -> Ivory eff ()
--          spiEmit h l = do
--            r <- local $ spi_req h l
--            emit req_e (constRef r)
--      callback $ \_ -> do
--        --x <- local $ ival 0
--        --let x = fromRep $ withBits 0 $ do
--        --            setBit drv_rw
--        --
--        let x = w_c2
--        let (l, h) = (bitCast $ toRep x, bitCast $ (toRep x) `iShiftR` 8)
--
--        let y = r_c2
--        let (l2, h2) = (bitCast $ toRep y, bitCast $ (toRep y) `iShiftR` 8)
--
--        --puts o "\n\rsnd\n\r"
--
--        wr <- deref write
--        --ifte_ wr (puts o "w" >> putc o h >> putc o l) (puts o "r" >> putc o h2 >> putc o l2)
--        --ifte_ wr (puts o "w") (puts o "r")
--        ifte_ wr (store write false >> spiEmit h l) (store write true >> spiEmit h2 l2)
--
--    handler res "spiResult" $ do
--      o <- emitter ostream 64
--      callback $ \r -> do
--        --code <- deref (r ~> resultcode)
--        --len <- deref (r ~> rx_idx)
--        --puts o "i"
--
--        --putc o (48 + (castWith 0 $ fromIx len))
--        arrayMap $ \ix -> do
--          when (fromIx ix <? 2) $ do
--            val <- deref ((r ~> rx_buf) ! ix)
--            --putc o (48 + (castWith 0 $ fromIx $ ix))
--            putc o val
--        --puts o "\n\r/rcv\n\r"

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral $ ord c)

uartTestTypes :: Module
uartTestTypes = package "uartTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)
