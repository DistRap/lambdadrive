
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import ODrive.Platforms
import ODrive.Tests.SPI (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
        app (stm32config_clock . testplatform_stm32)
            testplatform_spi
            testplatform_uart
            testplatform_leds
  where
  p topts = getConfig topts testPlatformParser
