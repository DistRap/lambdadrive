{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module LDrive.Platforms where
--  ( testPlatformParser
--  , ColoredLEDs(..)
--  , TestUART(..)
--  , TestSPI(..)
--  , TestCAN(..)
--  , TestDMA(..)
--  , TestPlatform(..)
--  , testplatform_clockconfig
--  , odrive
--  , drv8301
--  , drv8301_en_gate
--  , m1_ncs
--  , pinOut
--  ) where

import Ivory.Language
import Ivory.Tower.Config
import Data.Char (toUpper)

import qualified Ivory.BSP.STM32F405.ADC         as F405
import qualified Ivory.BSP.STM32F405.ATIM18      as F405
import qualified Ivory.BSP.STM32F405.CAN         as F405
import qualified Ivory.BSP.STM32F405.UART        as F405
import qualified Ivory.BSP.STM32F405.GPIO        as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF     as F405
import qualified Ivory.BSP.STM32F405.SPI         as F405
import qualified Ivory.BSP.STM32F405.RNG         as F405
import qualified Ivory.BSP.STM32F405.GTIM2345    as F405
import qualified Ivory.BSP.STM32F405.Interrupt   as F405

import Ivory.BSP.STM32.Peripheral.ADC
import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Peripheral.SPI as SPI -- hiding (ActiveHigh, ActiveLow)
import Ivory.BSP.STM32.Peripheral.RNG
import Ivory.BSP.STM32.Peripheral.UART.DMA
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Config
import Ivory.BSP.STM32.Interrupt

import LDrive.LED as LED

import Ivory.Tower.Drivers.PWM.ATIM

testPlatformParser :: ConfigParser TestPlatform
testPlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "ODRIVE"       -> result odrive
    "CAN4DISCO"    -> result c4d
    _ -> fail ("no such platform " ++ p)

  where
  result platform = do
    conf <- stm32ConfigParser (testplatform_stm32 platform)
    return platform { testplatform_stm32 = conf }

data ColoredLEDs =
  ColoredLEDs
    { redLED   :: LED
    , greenLED :: LED
    }

data TestUART =
  TestUART
    { testUARTPeriph :: UART
    , testUARTPins   :: UARTPins
    }

data TestSPI =
  TestSPI
    { testSPIPeriph :: SPIPeriph
    , testSPIPins   :: SPIPins
    -- TODO FIXME: move CS pins for test devices into TestSPI
    }

data TestCAN =
  TestCAN
    { testCAN        :: CANPeriph
    , testCANRX      :: GPIOPin
    , testCANTX      :: GPIOPin
    , testCANFilters :: CANPeriphFilters
    }

data TestDMA =
  TestDMA
    { testDMAUARTPeriph :: DMAUART
    , testDMAUARTPins   :: UARTPins
    }

data ADC = ADC {
      adcId      :: Uint8
    , adcPeriph  :: ADCPeriph
    , adcChan    :: (Uint8, GPIOPin)
    , adcInjChan :: (Uint8, GPIOPin)
    , adcInt     :: HasSTM32Interrupt
    }

data Enc = EncTimer {
      encTim :: F405.GTIM16
    , encChan1  :: GPIOPin
    , encChan2  :: GPIOPin
    , encAf     :: GPIO_AF
    }

data ExtInt =
  ExtInt
  { extInt :: HasSTM32Interrupt,
    extPin :: GPIOPin
  }

type ADCs = (ADC, ADC, ADC)

data TestPlatform =
  TestPlatform
    { testplatform_leds  :: ColoredLEDs
    , testplatform_uart  :: TestUART
    , testplatform_spi   :: TestSPI
    , testplatform_can   :: TestCAN
    , testplatform_rng   :: RNG
    , testplatform_stm32 :: STM32Config
    , testplatform_enc   :: Enc
    , testplatform_pwm   :: PWMTimer
    , testplatform_adc1  :: ADC
    , testplatform_adc2  :: ADC
    , testplatform_adc3  :: ADC
    , testplatform_adcs  :: ADCs
    }

testplatform_clockconfig :: TestPlatform -> ClockConfig
testplatform_clockconfig = stm32config_clock . testplatform_stm32

--testExti :: ExtInt
--testExti = ExtInt (HasSTM32Interrupt F405.EXTI0) F405.pinD1

testExti :: ExtInt
testExti = ExtInt (HasSTM32Interrupt F405.EXTI4) gpio3

adcint :: HasSTM32Interrupt
adcint = HasSTM32Interrupt F405.ADC
adc1, adc2, adc3 :: ADC
adc1 = ADC 1 F405.adc1 (5, F405.pinA5) (0, F405.pinA0) adcint
adc2 = ADC 2 F405.adc2 (13, F405.pinC3) (10, F405.pinC0) adcint
adc3 = ADC 3 F405.adc3 (12, F405.pinC2) (11, F405.pinC1) adcint

spi3_pins :: SPIPins
spi3_pins = SPIPins
  { spiPinMiso = F405.pinC12
  , spiPinMosi = F405.pinC11
  , spiPinSck  = F405.pinC10
  , spiPinAF   = F405.gpio_af_spi3
  }

gpio1, gpio2, gpio3, gpio4 :: GPIOPin
gpio1 = F405.pinB2
gpio2 = F405.pinA5
gpio3 = F405.pinA4
gpio4 = F405.pinA3

drv8301_en_gate :: GPIOPin
drv8301_en_gate = F405.pinB12

m0_dc_cal, m1_dc_cal :: GPIOPin
m0_dc_cal = F405.pinC9
m1_dc_cal = F405.pinC15

m0_nCS :: GPIOPin
m0_nCS = F405.pinC13
m1_nCS :: GPIOPin
m1_nCS = F405.pinC14

enc0 :: Enc
enc0 = EncTimer F405.tim3 F405.pinB4 F405.pinB5 F405.gpio_af_tim3

enc0Z0 :: GPIOPin
enc0Z0 = F405.pinA15

enc1 :: Enc
enc1 = EncTimer F405.tim4 F405.pinB6 F405.pinB7 F405.gpio_af_tim4

enc1Z0 :: GPIOPin
enc1Z0 = F405.pinB3

pwm0 :: PWMTimer
pwm0 = PWMTimer F405.tim1
    F405.pinA8 F405.pinA9 F405.pinA10
    F405.pinB13 F405.pinB14 F405.pinB15
    F405.gpio_af_tim1 0 tim_period_clocks

pwm1 :: PWMTimer
pwm1 = PWMTimer F405.tim8
    F405.pinC6 F405.pinC7 F405.pinC8
    F405.pinA7 F405.pinB0 F405.pinB1
    F405.gpio_af_tim8 0 tim_period_clocks

drv8301M0 :: SPIDevice
drv8301M0 =  SPIDevice
    { spiDevPeripheral    = F405.spi3
    , spiDevCSPin         = m0_nCS
    , spiDevClockHz       = 500000
    , spiDevCSActive      = SPI.ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase2
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "drv8301m0"
    }

drv8301M1 :: SPIDevice
drv8301M1 =  SPIDevice
    { spiDevPeripheral    = F405.spi3
    , spiDevCSPin         = m1_nCS
    , spiDevClockHz       = 500000
    , spiDevCSActive      = SPI.ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase2
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "drv8301m1"
    }


tim_period_clocks :: Uint16
tim_period_clocks = 8192

currentMeasPeriod :: ClockConfig -> IFloat
currentMeasPeriod cc = (2 * (safeCast tim_period_clocks) / (fromIntegral pclkhz))
  where
    pclkbus = PClk2
    pclkhz = clockPClkHz pclkbus cc

currentMeasHz :: ClockConfig -> IFloat
currentMeasHz cc = (fromIntegral pclkhz) / (safeCast $ 2 * tim_period_clocks)
  where
    pclkbus = PClk2
    pclkhz = clockPClkHz pclkbus cc

odrive :: TestPlatform
odrive = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED   = LED gpio1 LED.ActiveHigh
      , greenLED = LED gpio2 LED.ActiveHigh
      }
  , testplatform_uart = TestUART
    { testUARTPeriph = F405.uart1
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinB6
        , uartPinRx = F405.pinB7
        , uartPinAF = F405.gpio_af_uart1
        }
    }
  , testplatform_spi = TestSPI
    { testSPIPeriph = F405.spi3
    , testSPIPins   = spi3_pins
    }
  , testplatform_can = TestCAN
      { testCAN = F405.can1
      , testCANRX = F405.pinB8
      , testCANTX = F405.pinB9
      , testCANFilters = F405.canFilters
      }
  , testplatform_rng = F405.rng
  , testplatform_enc = enc0
  , testplatform_pwm = pwm0
  , testplatform_adc1 = adc1
  , testplatform_adc2 = adc2
  , testplatform_adc3 = adc3
  , testplatform_adcs = (adc1, adc2, adc3)

  , testplatform_stm32 = odriveSTMConfig 8
  }

c4d :: TestPlatform
c4d = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED   = LED F405.pinD14 LED.ActiveHigh
      , greenLED = LED F405.pinD15 LED.ActiveHigh
      }
  , testplatform_uart = TestUART
    { testUARTPeriph = F405.uart2
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinA2
        , uartPinRx = F405.pinA3
        , uartPinAF = F405.gpio_af_uart2
        }
    }
  , testplatform_spi = TestSPI
    { testSPIPeriph = F405.spi3
    , testSPIPins   = spi3_pins
    }
  , testplatform_can = TestCAN
      { testCAN = F405.can1
      , testCANRX = F405.pinB8
      , testCANTX = F405.pinB9
      , testCANFilters = F405.canFilters
      }
  , testplatform_rng = F405.rng
  , testplatform_enc = enc0
  , testplatform_pwm = pwm0
  , testplatform_adc1 = adc1
  , testplatform_adc2 = adc2
  , testplatform_adc3 = adc3
  , testplatform_adcs = (adc1, adc2, adc3)

  , testplatform_stm32 = odriveSTMConfig 8
  }

--- XXX: clock hackery, suggest upstream

data Divs = Divs {
    div_hclk :: Integer
  , div_pclk1 :: Integer
  , div_pclk2 :: Integer
  }

externalXtalDivs :: Integer -> Integer -> Divs -> ClockConfig
externalXtalDivs xtal_mhz sysclk_mhz Divs{..} = ClockConfig
  { clockconfig_source = External (xtal_mhz * 1000 * 1000)
  , clockconfig_pll    = PLLFactor
      { pll_m = xtal_mhz
      , pll_n = sysclk_mhz * 2
      , pll_p = 2
      , pll_q = 7
      }
  , clockconfig_hclk_divider = div_hclk
  , clockconfig_pclk1_divider = div_pclk1
  , clockconfig_pclk2_divider = div_pclk2
  }

-- STM32F405RGT6
odriveSTMConfig :: Integer -> STM32Config
odriveSTMConfig xtal_mhz = STM32Config
  { stm32config_processor  = STM32F405
  , stm32config_px4version = Nothing
  , stm32config_clock      = externalXtalDivs xtal_mhz 168 divs
  -- XXX: this is 192 in total (112+16+64)
  -- 64 is CCM (core coupled memory)
  -- + 4kb additional backup sram
--  , stm32config_sram       = 128 * 1024
  , stm32config_sram       = 164 * 1024
  }
  where
    divs = Divs
             { div_hclk = 1
             , div_pclk1 = 2
             , div_pclk2 = 1
             }
