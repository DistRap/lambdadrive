{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ODrive.Utils where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.Peripheral.GPIOF4

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral $ ord c)

puts :: (GetAlloc eff ~ 'Scope cs)
     => Emitter ('Stored Uint8) -> String -> Ivory eff ()
puts e str = mapM_ (\c -> putc e (fromIntegral (ord c))) str

putc :: (GetAlloc eff ~ 'Scope cs)
     => Emitter ('Stored Uint8) -> Uint8 -> Ivory eff ()
putc = emitV

uartUnbuffer :: forall b e
              . (IvoryString b)
             => BackpressureTransmit b ('Stored IBool)
             -> Tower e (ChanInput ('Stored Uint8))
uartUnbuffer (BackpressureTransmit req res) = do
  c <- channel
  p <- period (Milliseconds 10)
  monitor "uart_unbuffer" $ do
    flush_defer <- state "flush_defer"
    buf <- state "buffer"
    let ready_buf :: Ivory eff IBool
        ready_buf = fmap (>? 0) (deref (buf ~> stringLengthL))

        send_buf :: Emitter b -> Ivory eff ()
        send_buf e = do
          emit e (constRef buf)
          store (buf ~> stringLengthL) 0
          store flush_defer true

    handler (snd c) "uart_byte_tosend" $ do
      callbackV $ \byte -> do
        pos <- deref (buf ~> stringLengthL)
        when (pos <? arrayLen (buf ~> stringDataL)) $ do
          store (buf ~> stringDataL ! toIx pos) byte
          store (buf ~> stringLengthL) (pos + 1)

    handler p "uart_tx_flush" $ do
      e <- emitter req 1
      callback $ const $ do
        defer <- deref flush_defer
        ready <- ready_buf
        when (ready .&& iNot defer) (send_buf e)

    handler res "uart_tx_res" $ do
      callback $ const $ store flush_defer false

  return (fst c)

-- blink on `pin` on every `chan` input
debugTower pin chan = do
  monitor "debug" $ do
    handler systemInit "debugInit" $ do
      callback $ const $ pinOut pin
    handler chan "debug" $ do
      callback $ const $ do
        sequence_ $ take 6 $ repeat $ pinHigh pin
        pinLow pin

pinOut :: GPIOPin -> Ivory eff()
pinOut pin = do
  pinEnable pin
  pinSetOutputType pin gpio_outputtype_pushpull
  pinSetSpeed pin gpio_speed_2mhz
  pinSetPUPD pin gpio_pupd_none

pinLow :: GPIOPin -> Ivory eff ()
pinLow pin = do
  pinClear pin
  pinSetMode pin gpio_mode_output

pinHigh :: GPIOPin -> Ivory eff ()
pinHigh pin = do
  pinSet pin
  pinSetMode pin gpio_mode_output

pinHiZ :: GPIOPin -> Ivory eff ()
pinHiZ pin = pinSetMode pin gpio_mode_analog
