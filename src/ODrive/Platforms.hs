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

import qualified Ivory.BSP.STM32F405.ADC         as F405
import qualified Ivory.BSP.STM32F405.ATIM18      as F405
import qualified Ivory.BSP.STM32F405.CAN         as F405
import qualified Ivory.BSP.STM32F405.UART        as F405
import qualified Ivory.BSP.STM32F405.GPIO        as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF     as F405
import qualified Ivory.BSP.STM32F405.SPI         as F405
import qualified Ivory.BSP.STM32F405.I2C         as F405
import qualified Ivory.BSP.STM32F405.RNG         as F405
import qualified Ivory.BSP.STM32F405.GTIM2345    as F405
import qualified Ivory.BSP.STM32F405.Interrupt   as F405

import Ivory.BSP.STM32.Peripheral.ADC
import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Peripheral.SPI as SPI -- hiding (ActiveHigh, ActiveLow)
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.RNG
import Ivory.BSP.STM32.Peripheral.UART.DMA
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Config
import Ivory.BSP.STM32.Interrupt

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

data PWM = PWMTimer {
      pwmTim  :: F405.ATIM
    , pwmC1   :: GPIOPin
    , pwmC2   :: GPIOPin
    , pwmC3   :: GPIOPin
    , pwmC1N  :: GPIOPin
    , pwmC2N  :: GPIOPin
    , pwmC3N  :: GPIOPin
    , pwmAf   :: GPIO_AF
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
    , testplatform_pwm   :: PWM
    , testplatform_adc1  :: ADC
    , testplatform_adc2  :: ADC
    , testplatform_adc3  :: ADC
    , testplatform_adcs  :: ADCs
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
--
-- |ADCs
-- PA0  ADC_IN0   VBUS_S
-- PA1  ADC_IN1   M1_TEMP
-- PA3  ADC_IN3   GPIO4
-- PA4  ADC_IN4   GPIO3
-- PA5  ADC_IN5   GPIO2
-- PA6  ADC_IN6   AUX_V
-- PC0  ADC_IN10  M0_SO1
-- PC1  ADC_IN11  M0_SO2
-- PC2  ADC_IN12  M1_SO1
-- PC3  ADC_IN13  M1_SO2
-- PC4  ADC_IN14  AUX_TEMP
-- PC5  ADC_IN15  M0_TEMP
--
-- ADCs - divider 4, 12 bit, 3 samples
-- ADC1 - sw start IN5 injected IN0 (VBUS_S)
--                              ^^ T1 CC4, rising
-- ADC2 - switches between injected IN10 (M0_SO1) and IN13 (M1_SO2)
-- ADC3 - switches between injected IN11 (M0_SO2) and IN12 (M1_SO1)
--      - both also switch to DC cals in the same manner ^^
--
-- M0 triggered on both ADCS by rising T1 CC4, then switches to T1 TRGO for DC cal
-- M1 triggered on both ADCS by rising T8 CC4, then switches to regular conversion for DC cal
--
-- M0_DC_CAL PC9
-- M1_DC_CAL PC1
--
-- rise = ADC_EXTERNALTRIGCONVEDGE_RISING = ADC_CR2_EXTEN_0 = 0x1
--
-- ADC2 interrupt comes first, followed by ADC3
--
-- if it's ADC2 -> phase B
-- else    ADC3 -> phase C
--
-- if CR2_EXTEN != ADC_EXTERNALTRIGCONVEDGE_NONE
-- -> M1 DC_CAL
--    reset M1_DC_CAL pin (next on M1 is current)
--    set CR2 ADC_EXTERNALTRIGINJECCONV_T1_CC4 rise (next on ADC is M0 current)
--    if adc2 set ADC_IN10
--    if adc3 set ADC_IN11
-- else if CR2_JEXTSEL == ADC_EXTERNALTRIGINJECCONV_T1_CC4
-- -> M0 current
--    set M0_DC_CAL pin (next on M0 is DC_CAL)
--    set CR2 ADC_EXTERNALTRIGINJECCONV_T1_CC8 rise (next on ADC is M1 current)
--    if adc2 set ADC_IN13
--    if adc3 set ADC_IN12
-- else if CR2_JEXTSEL == ADC_EXTERNALTRIGINJECCONV_T8_CC4
-- -> M1 current
--    set M1_DC_CAL pin (next on M1 is DC_CAL)
--    set CR2 ADC_EXTERNALTRIGINJECCONV_T1_TRGO rise (next on ADC is M0 DC_CAL)
--    if adc2 set ADC_IN10
--    if adc3 set ADC_IN11
-- else if CR2_JEXTSEL == ADC_EXTERNALTRIGINJECCONV_T1_TRGO
-- -> M0 DC_CAL
--    reset M0_DC_CAL pin (next on M0 is current)
--    clear CR2, set rise (next on ADC is M1 DC_CAL)
--    if adc2 set ADC_IN13
--    if adc3 set ADC_IN12

adcint :: HasSTM32Interrupt
adcint = HasSTM32Interrupt F405.ADC
adc1, adc2, adc3 :: ADC
adc1 = ADC 1 F405.adc1 (5, F405.pinA5) (0, F405.pinA0) adcint
adc2 = ADC 2 F405.adc2 (13, F405.pinC3) (10, F405.pinC0) adcint
adc3 = ADC 3 F405.adc3 (12, F405.pinC2) (11, F405.pinC1) adcint

m0dcCal :: GPIOPin
m0dcCal = F405.pinC9
m1dcCal :: GPIOPin
m1dcCal = F405.pinC1

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
m1_dc_cal = F405.pinC1

m0_nCS :: GPIOPin
m0_nCS = F405.pinC13
m1_nCS :: GPIOPin
m1_nCS = F405.pinC14

enc0 :: Enc
enc0 = EncTimer F405.tim3 F405.pinB4 F405.pinB5 F405.gpio_af_tim3

enc1 :: Enc
enc1 = EncTimer F405.tim4 F405.pinB6 F405.pinB7 F405.gpio_af_tim4

pwm0 :: PWM
pwm0 = PWMTimer F405.tim1
    F405.pinA8 F405.pinA9 F405.pinA10
    F405.pinB13 F405.pinB14 F405.pinB15
    F405.gpio_af_tim1

pwm1 :: PWM
pwm1 = PWMTimer F405.tim8
    F405.pinC6 F405.pinC7 F405.pinC8
    F405.pinA7 F405.pinB0 F405.pinB1
    F405.gpio_af_tim8

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
-- f4 disco bridged
  , testplatform_uart = TestUART
    { testUARTPeriph = F405.uart2
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinA2
        , uartPinRx = F405.pinA3
        , uartPinAF = F405.gpio_af_uart2
        }
    }
-- odrive via encoder 2
--  , testplatform_uart = TestUART
--    { testUARTPeriph = F405.uart1
--    , testUARTPins = UARTPins
--        { uartPinTx = F405.pinB6
--        , uartPinRx = F405.pinB7
--        , uartPinAF = F405.gpio_af_uart1
--        }
--    }
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

  , testplatform_stm32 = stm32f405Defaults 8
  }

pinOut :: GPIOPin -> Ivory eff()
pinOut pin = do
  pinEnable pin
  pinSetOutputType pin gpio_outputtype_pushpull
  pinSetSpeed pin gpio_speed_2mhz
  pinSetPUPD pin gpio_pupd_none
