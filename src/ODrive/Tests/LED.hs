{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module ODrive.Tests.LED where

import Ivory.Tower
import ODrive.Platforms
import ODrive.Tests.Blink

app :: (e -> ColoredLEDs) -> Tower e ()
app toleds = do
  leds <- fmap toleds getEnv
  blink (Milliseconds 250) [redLED leds]
  blink (Milliseconds 333) [blueLED leds]