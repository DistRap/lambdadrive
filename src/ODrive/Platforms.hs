{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module ODrive.Platforms where
--  ( testPlatformParser
--  , ColoredLEDs(..)
--  , TestUART(..)
--  , TestSPI(..)
--  , TestI2C(..)
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

import qualified Ivory.BSP.STM32F405.CAN         as F405
import qualified Ivory.BSP.STM32F405.UART        as F405
import qualified Ivory.BSP.STM32F405.GPIO        as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF     as F405
import qualified Ivory.BSP.STM32F405.SPI         as F405
import qualified Ivory.BSP.STM32F405.I2C         as F405
import qualified Ivory.BSP.STM32F405.RNG         as F405
import qualified Ivory.BSP.STM32F405.GTIM2345    as F405

import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Peripheral.SPI as SPI -- hiding (ActiveHigh, ActiveLow)
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.RNG
import Ivory.BSP.STM32.Peripheral.UART.DMA
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Config

import ODrive.LED as LED

testPlatformParser :: ConfigParser TestPlatform
testPlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "ODRIVE"       -> result odrive
    _ -> fail ("no such platform " ++ p)

  where
  result platform = do
    conf <- stm32ConfigParser (testplatform_stm32 platform)
    return platform { testplatform_stm32 = conf }

data ColoredLEDs =
  ColoredLEDs
    { redLED  :: LED
    , blueLED :: LED
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

data TestI2C =
  TestI2C
    { testI2C     :: I2CPeriph
    , testI2CPins :: I2CPins
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

data Enc = EncTimer {
      encTim :: F405.GTIM16
    , encChan1  :: GPIOPin
    , encChan2  :: GPIOPin
    , encAf     :: GPIO_AF
    }

data TestPlatform =
  TestPlatform
    { testplatform_leds  :: ColoredLEDs
    , testplatform_uart  :: TestUART
    , testplatform_spi   :: TestSPI
    , testplatform_can   :: TestCAN
    , testplatform_rng   :: RNG
    , testplatform_stm32 :: STM32Config
    , testplatform_enc   :: Enc
    }

testplatform_clockconfig :: TestPlatform -> ClockConfig
testplatform_clockconfig = stm32config_clock . testplatform_stm32

-- ODrive
--
--
-- GPIO1 PB2
-- GPIO2 PA5
-- GPIO3 PA4
-- GPIO4 PA3
--
-- === M0 ===
--
-- M0 AH PA8
-- M0 BH PA9
-- M0 CH PA10
--
-- M0 AL PB13
-- M0 BL PB14
-- M0 CL PB15
--
-- M0 ENC_A PB4
-- M0 ENC_B PB5
-- M0 ENC_Z PA15
--
-- M0 SO1 PC0
-- M0 SO2 PC1
--
-- M0 TEMP PC5
-- M0 DC_CAL PC9
--
-- M0 nCS PC13
--
-- === M1 ===
--
-- M1 AH PC6
-- M1 BH PC7
-- M1 CH PC8
--
-- M1 AL PA7
-- M1 BL PB0
-- M1 CL PB1
--
-- M1 ENC_A PB6
-- M1 ENC_B PB7
-- M1 ENC_Z PB3
--
-- M1 SO1 PC2
-- M1 SO2 PC3
--
-- M1 TEMP PA1
-- M1 DC_CAL PC15
-- M1 nCS PC14
--
-- SPI_SCK PC10
-- SPI_MISO PC11
-- SPI_MOSI PC12
-- XXX: is MISO/MOSI flipped?
--
-- EN_GATE PB12
-- nFAULT PD2
-- VBUS-S PA0
--
-- |AUX
-- AUX_L PB10
-- AUX_H PB11
-- AUX_V PA6
-- AUX_TEMP PC4


-- || Peripherals
-- |TIM2 (AUX)
-- PB10 TIM2_CH3
-- PB11 TIM2_CH4
--
-- |TIM3 (E0)
-- PB4 TIM3_CH1
-- PB5 TIM3_CH2

-- |TIM4 (E1)
-- PB6 TIM4_CH1
-- PB7 TIM4_CH2
--
-- |TIM1 (M0)
-- PA8  TIM1_CH1
-- PA9  TIM1_CH2
-- PA10 TIM1_CH3
-- PB13 TIM1_CH1N
-- PB14 TIM1_CH2N
-- PB15 TIM1_CH3N
--
-- |TIM8 (M1)
-- PC6  TIM8_CH1
-- PC7  TIM8_CH2
-- PC8  TIM8_CH3
-- PA7  TIM8_CH1N
-- PB0  TIM8_CH2N
-- PB1  TIM8_CH3N

spi3_pins :: SPIPins
spi3_pins = SPIPins
  { spiPinMiso = F405.pinC12
  , spiPinMosi = F405.pinC11
  , spiPinSck  = F405.pinC10
  , spiPinAF   = F405.gpio_af_spi3
  }

gpio1 = F405.pinB2
gpio2 = F405.pinA5
gpio3 = F405.pinA4
gpio4 = F405.pinA3

drv8301_en_gate = F405.pinB12

m0_nCS = F405.pinC13
m1_nCS = F405.pinC14


enc0 = EncTimer F405.tim3 F405.pinB4 F405.pinB5 F405.gpio_af_tim4

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

odrive :: TestPlatform
odrive = TestPlatform
  { testplatform_leds = ColoredLEDs
      { redLED  = LED gpio1 LED.ActiveHigh
      , blueLED = LED gpio2 LED.ActiveHigh
      }
  , testplatform_uart = TestUART
    { testUARTPeriph = F405.uart1
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinB6
        , uartPinRx = F405.pinB7
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
  , testplatform_stm32 = stm32f405Defaults 8
  , testplatform_enc = enc0
  }

pinOut :: GPIOPin -> Ivory eff()
pinOut pin = do
  pinEnable pin
  pinSetOutputType pin gpio_outputtype_pushpull
  pinSetSpeed pin gpio_speed_2mhz
  pinSetPUPD pin gpio_pupd_none
