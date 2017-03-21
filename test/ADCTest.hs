
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import ODrive.Platforms
import ODrive.Tests.ADC (app)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
        app (stm32config_clock . testplatform_stm32)
            testplatform_adc
            testplatform_pwm
            testplatform_uart
            testplatform_leds
  where
  p topts = getConfig topts testPlatformParser
