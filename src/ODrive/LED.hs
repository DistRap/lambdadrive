{-# LANGUAGE DataKinds #-}

module ODrive.LED where

import Ivory.Language
import Ivory.Tower
import Ivory.HW.Module

import Ivory.BSP.STM32.Peripheral.GPIOF4

import ODrive.Utils

data LEDPolarity = ActiveHigh | ActiveLow
data LED = LED GPIOPin LEDPolarity

ledSetup :: LED -> Ivory eff ()
ledSetup led@(LED pin _polarity) = do
  pinEnable pin
  pinSetOutputType pin gpio_outputtype_pushpull
  pinSetSpeed pin gpio_speed_2mhz
  pinSetPUPD pin gpio_pupd_none
  ledOff led

ledOn :: LED -> Ivory eff ()
ledOn (LED pin ActiveHigh) = pinHigh pin
ledOn (LED pin ActiveLow)  = pinLow  pin

ledOff :: LED -> Ivory eff ()
ledOff (LED pin _) = pinHiZ pin

-- | LED Controller: Given a set of leds and a control channel of booleans,
--   setup the pin hardware, and turn the leds on when the control channel is
--   true.
ledController :: [LED] -> ChanOutput ('Stored IBool) -> Monitor e ()
ledController leds rxer = do
  -- Bookkeeping: this task uses Ivory.HW.Module.hw_moduledef
  monitorModuleDef $ hw_moduledef
  -- Setup hardware before running any event handlers
  handler systemInit "hardwareinit" $
    callback $ const $ mapM_ ledSetup leds
  -- Run a callback on each message posted to the channel
  handler rxer "newoutput" $ callback $ \outref -> do
    out <- deref outref
    -- Turn pins on or off according to event value
    ifte_ out
      (mapM_ ledOn  leds)
      (mapM_ ledOff leds)

-- | Blink task: Given a period and a channel source, output an alternating
--   stream of true / false on each period.
blinker :: Time a => a -> Tower e (ChanOutput ('Stored IBool))
blinker t = do
  p_chan <- period t
  (cin, cout) <- channel
  monitor "blinker" $ do
    handler p_chan "per" $  do
      e <- emitter cin 1
      callback $ \timeref -> do
        time <- deref timeref
        -- Emit boolean value which will alternate each period.
        emitV e (time .% (2*p) <? p)
  return cout
  where p = toITime t

blink :: Time a => a -> [LED] -> Tower p ()
blink per pins = do
  onoff <- blinker per

--  monitor "setup" $ do
--     handler systemInit "init" $ do
--      callback $ const $ do
--        mapM_ ledSetup pins

  monitor "led" $ ledController pins onoff
