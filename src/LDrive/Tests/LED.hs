{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module LDrive.Tests.LED where

import Ivory.Tower
import LDrive.Platforms
import LDrive.LED

app :: (e -> ColoredLEDs) -> Tower e ()
app toleds = do
  leds <- fmap toleds getEnv
  blink (Milliseconds 1000) [redLED leds]
  blink (Milliseconds 666) [greenLED leds]
