{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ODrive.DRV8301
  ( module ODrive.DRV8301.Regs
  , module ODrive.DRV8301.RegTypes
  , drvTower
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.BSP.STM32.Driver.SPI

import ODrive.DRV8301.Regs
import ODrive.DRV8301.RegTypes
import ODrive.Platforms
import ODrive.Utils

-- DRV8301 driver
drvTower :: (IvoryZero init, IvoryArea init) =>
            BackpressureTransmit
              ('Struct "spi_transaction_request")
              ('Struct "spi_transaction_result")
            -> ChanOutput init
            -> SPIDeviceHandle
            -> Tower e (( ChanOutput ('Stored IBool)
                        , ChanOutput ('Struct "drv_fault") ))
drvTower (BackpressureTransmit req_c res_c) init_chan dev = do
  periodic <- period (Milliseconds 1000)
  ready_chan <- channel
  fault_chan <- channel

  monitor "drv8301mon" $ do
    initok <- stateInit "initok" (ival false)
    drvReady <- stateInit "drvReady" (ival false)
    retries <- stateInit "retries" (ival (0 :: Uint8))

    readStatus1 <- stateInit "readStatus1" (ival true)
    lastStatus1 <- stateInit "lastStatus1" (ival (0 :: Uint16))
    lastStatus2 <- stateInit "lastStatus2" (ival (0 :: Uint16))
    lastStat <- stateInit "lastStat" (istruct [])
    handler systemInit "init" $ do
      callback $ const $ do
        -- enable en_gate pin connected to both DRVs
        pinOut drv8301_en_gate
        pinHigh drv8301_en_gate

    coroutineHandler init_chan res_c "drv8301" $ do
      req_e <- emitter req_c 1
      ready_e <- emitter (fst ready_chan) 1
      fault_e <- emitter (fst fault_chan) 1
      return $ CoroutineBody $ \ yield -> do
        -- rpc is nice, you emit request and yield
        -- while your request is processed
        --let rpc req = req >>= emit req_e >> yield
        -- but IT RESPONDS IN THE NEXT CYCLE!!!
        -- so we emit twice and return the second reply
        --let rpc req = req >>= emit req_e >> yield >> req >>= emit req_e >> yield
        -- which is ~equal to
        let rpc req = do
              newreq <- req
              emit req_e newreq
              _ <- yield -- previous command response
              emit req_e newreq
              yield -- our response

        let c1data = repToBits $ withBits 0 $ do
              setField drv_ocmode drv_ocmode_latch_shutdown
              setField drv_ocadj drv_ocadj_0p730

            c1write = drv_write_control1 c1data

        let c2data = repToBits $ withBits 0 $ do
              setField drv_gain drv_gain_40

            c2write = drv_write_control2 c2data

        let rxbufToUint16 buf = do
              h <- fmap (safeCast :: Uint8 -> Uint16) $ deref (buf ! 0)
              l <- fmap (safeCast :: Uint8 -> Uint16) $ deref (buf ! 1)
              return $ (h `iShiftL` 8) + l

        comment "Initialization routine"
        comment "Loop until register changes responding to our write or we reach retries"
        forever $ do
          _ <- noBreak $ rpc (spi_req dev c1write)
          r <- noBreak $ rpc (spi_req dev drv_read_control1)

          -- compare if value we write matches with readout
          rval <- rxbufToUint16 (r ~> rx_buf)
          when (rval ==? (toRep c1write)) $ do
            store initok true
            breakOut

          rs <- deref retries
          store retries (rs + 1)

          -- abort if initialization timed out
          when (rs >? 250) retVoid

        isinitok <- deref initok
        when isinitok $ do
          -- write control2 and verify
          _ <- rpc (spi_req dev $ drv_write_control2 c2data)
          r2 <- rpc (spi_req dev drv_read_control2)
          r2val <- rxbufToUint16 (r2 ~> rx_buf)
          assert (r2val ==? (toRep c2write))

          -- let others know we're initialized
          store drvReady true
          emitV ready_e true

          comment "handle periodic reads"
          forever $ do
            comment "wait for response triggered by periodic request"
            res <- yield
            regval <- rxbufToUint16 (res ~> rx_buf)

            rs1 <- deref readStatus1
            ifte_ rs1
              (store lastStatus1 regval)
              (do store lastStatus2 regval
                  ls1 <- deref lastStatus1
                  ls2 <- deref lastStatus2
                  -- ls2 contains only one interesting bit (bit 7)
                  when (ls1 /=? 0 .|| (ls2 .& 0x80) /=? 0) $ do
                    drvFaultFromRegs ls1 ls2 lastStat
                    emit fault_e $ constRef lastStat
                  )

    handler periodic "periodic" $ do
      req_e <- emitter req_c 1
      callback $ \_ -> do
        isReady <- deref drvReady
        when isReady $ do
          rs1 <- deref readStatus1
          ifte_ rs1
            ((spi_req dev drv_read_status1) >>= emit req_e >> store readStatus1 false)
            ((spi_req dev drv_read_status2) >>= emit req_e >> store readStatus1 true)

  return (snd ready_chan, snd fault_chan)

-- read/write shortcuts for both control and status registers
drv_write_control1 :: Bits 11 -> Drv8301
drv_write_control1 val = drv_msg drv_write drv_control1 val

drv_write_control2 :: Bits 11 -> Drv8301
drv_write_control2 val = drv_msg drv_write drv_control2 val

drv_read_control1 :: Drv8301
drv_read_control1 = drv_msg_read drv_control1

drv_read_control2 :: Drv8301
drv_read_control2 = drv_msg_read drv_control2

drv_read_status1 :: Drv8301
drv_read_status1 = drv_msg_read drv_status1

drv_read_status2 :: Drv8301
drv_read_status2 = drv_msg_read drv_status2

-- build a DRV8301 register read message
drv_msg_read :: DrvAddr -> Drv8301
drv_msg_read addr = drv_msg drv_read addr (fromRep 0)

-- build a DRV8301 control message
drv_msg :: DrvRW -> DrvAddr -> Bits 11 -> Drv8301
drv_msg rw addr c = fromRep $ withBits 0 $ do
  setField drv_rw rw
  setField drv_addr addr
  setField drv_data c

-- create an SPI request from device and Drv8301 data
spi_req :: (GetAlloc eff ~ 'Scope s)
        => SPIDeviceHandle
        -> Drv8301
        -> Ivory eff (ConstRef ('Stack s) ('Struct "spi_transaction_request"))
spi_req dev msg = fmap constRef $ local $ istruct
              [ tx_device .= ival dev
              , tx_buf    .= iarray [h msg, l msg]
              , tx_len    .= ival 2
              ]
  where l x = ival $ bitCast $ toRep x
        h x = ival $ bitCast $ (toRep x) `iShiftR` 8
