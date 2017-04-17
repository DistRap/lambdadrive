{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ODrive.Tests.CAN2UART where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Peripheral.CAN.Filter

import ODrive.Platforms
import ODrive.LED
import ODrive.Types
import ODrive.Utils

canSend' :: AbortableTransmit ('Struct "can_message") ('Stored IBool)
         -> ChanOutput  ('Struct "can_message") -- ('Array 8 ('Stored Uint8)))
         -> Tower p ()
canSend' req msg = do
    monitor "canSender" $ do
      tx_pending <- state "tx_pending"
      last_sent  <- state "last_sent"
      handler msg "can_msg" $ do
        abort_emitter <- emitter (abortableAbort    req) 1
        req_emitter   <- emitter (abortableTransmit req) 1
        callback $ \msg  -> do
          refCopy last_sent msg

          was_pending <- deref tx_pending
          ifte_ was_pending (emitV abort_emitter true) $ do
            emit req_emitter $ constRef last_sent
            store tx_pending true

      handler (abortableComplete req) "tx_complete" $ do
        req_emitter <- emitter (abortableTransmit req) 1
        callbackV $ \ ok -> do
          ifte_ ok (store tx_pending false) $ do
            emit req_emitter $ constRef last_sent
            store tx_pending true

app :: (e -> ClockConfig)
    -> (e -> TestCAN)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestcan touart toleds = do
  towerDepends odriveTypes
  towerModule  odriveTypes

  can  <- fmap totestcan getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv

  (canctl_input, canctl_output) <- channel

  (buffered_ostream, istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200

  monitor "dma" mon
  -- UART buffer transmits in buffers. We want to transmit byte-by-byte and let
  -- this monitor manage periodically flushing a buffer.
  ostream <- uartUnbuffer (buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))
  echoPrompt "hello world" ostream istream canctl_input

  (res, req, _, _) <- canTower tocc (testCAN can) 1000000 (testCANRX can) (testCANTX can)

  -- periodic <- period (Milliseconds 250)

  canSend' req canctl_output

  monitor "simplecontroller" $ do
    handler systemInit "init" $ do
      callback $ const $ do
        let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
        canFilterInit (testCANFilters can)
                      [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID]
                      []
        ledSetup $ redLED leds
        ledSetup $ blueLED leds

    received <- stateInit "can_received_count" (ival (0 :: Uint32))

    handler res "result" $ do
      o <- emitter ostream 64
      callback $ \msg -> do
        --(ledOn  $ blueLED leds)
        count <- deref received
        store received (count + 1)
        puts o "\n\rrcv\n\r"
        -- l <- deref (msg ~> can_message_len)
        arrayMap $ \ix -> do
            --when (fromIx ix <? 8) $ do
              val <- deref ((msg ~> can_message_buf) ! ix)
              putc o (48 + (castWith 0 $ fromIx $ ix))
              putc o val
        puts o "\n\r/rcv\n\r"

        ifte_ (count .& 1 ==? 1)
          (ledOff $ blueLED leds)
          (ledOn  $ blueLED leds)

echoPrompt :: String
           -> ChanInput  ('Stored Uint8)
           -> ChanOutput ('Stored Uint8)
           -> ChanInput  ('Struct "can_message")
           -> Tower p ()
echoPrompt greeting ostream istream canctl = do
  towerDepends canDriverTypes
  towerModule  canDriverTypes
  p <- period (Milliseconds 1)


  monitor "echoprompt" $ do
    (incoming :: Ref 'Global UARTBuffer) <- state "incoming"
    initialized <- stateInit "initialized" (ival false)

    let push :: Uint8 -> Ivory eff ()
        push byte = do
          pos <- deref (incoming ~> stringLengthL)
          when (pos <? arrayLen (incoming ~> stringDataL)) $ do
            store (incoming ~> stringDataL ! toIx pos) byte
            store (incoming ~> stringLengthL) (pos + 1)

    handler p "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        i <- deref initialized
        unless i $ do
          store initialized true
          puts o (greeting ++ "\n")
          puts o prompt

    handler istream "istream" $ do
      c <- emitter canctl 1
      o <- emitter ostream 32
      callbackV $ \input -> do
        putc o input -- echo to terminal
        push input
        let testChar = (input `isChar`)
        pos <- deref (incoming ~> stringLengthL)
        --a <- local $ iarray [0..7]
        when (pos ==? 8) $ do
          --arrayMap $ \ix -> do
          --  when (fromIx ix <? 8) $ do
          --    val <- deref ((incoming ~> stringDataL) ! ix)
          --    putc o val

          let msgid = standardCANID (fromRep 0x7FF) (boolToBit false)
          r <- local $ istruct
            [ can_message_id  .= ival msgid
--            , can_message_buf .= deref buf
--                iarray [ ival $ bitCast $ time `iShiftR` fromInteger (8 * i) | i <- [7,6..0] ]
            , can_message_len .= ival 8
            ]
          -- arrayCopy: to from offset len
          arrayCopy (r ~> can_message_buf) (constRef (incoming ~> stringDataL)) 0 8
          emit c (constRef r)
          store (incoming ~> stringLengthL) 0

  where prompt = "tower> "
