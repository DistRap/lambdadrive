{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module ODrive.Tests.ADC where

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Peripheral.ADC
import Ivory.BSP.STM32.Peripheral.ATIM18
import Ivory.BSP.STM32.Peripheral.GPIOF4

import ODrive.Platforms
import ODrive.LED
import ODrive.PWM
import ODrive.Types
import ODrive.Utils

adcTower :: ADC -> Tower e ()
adcTower ADC {adcPeriph=adcp@ADCPeriph{..}, adcChan=chan, adcInjChan=ichan, adcInt=int} = do

  periodic <- period (Milliseconds 500)

  isr <- signalUnsafe
            (Interrupt int)
            (Microseconds 250)
            (interrupt_disable int)

  monitor "adc" $ do
    monitorModuleDef $ hw_moduledef

    lastadc <- stateInit "lastadc" (ival (0 :: Uint16))
    lastadci <- stateInit "lastadci" (ival (0 :: Uint16))
    lastjeoc <- stateInit "lastjeoc" (ival (0 :: Uint8))

    handler isr "adc_capture" $ do
      callback $ const $ do
        jdr1 <- getReg adcRegJDR1
        store lastadci (toRep (jdr1 #. adc_jdr1_data))

        comment "clear interrupt flags in status register"
        modifyReg adcRegSR $ do
          clearBit adc_sr_strt
          clearBit adc_sr_eoc

          clearBit adc_sr_jstrt
          clearBit adc_sr_jeoc

        interrupt_enable int

    handler systemInit "init" $ do
      callback $ const $ do
        mapM_ adc_in_pin $ map snd [chan, ichan]
        adcInit adcp adc_12bit false

        comment "prescaler PCLK2 DIV4 for all adcs"
        modifyReg adcRegCCR $ setField adc_ccr_adcpre adc_pclk2_div4

        comment "enable interrupts"
        modifyReg adcRegCR1 $ do
          setBit adc_cr1_eocie
          setBit adc_cr1_jeocie

        comment "eocs to single conversion mode"
        comment "software start, no external trigger"
        comment "injected - t1 cc4, rising"
        modifyReg adcRegCR2 $ do
          setBit adc_cr2_eocs
          setField adc_cr2_exten adc_exten_none

          -- setBit adc_cr2_swstart ??

          -- FIXME: needs passing inject trigger source and mode or too generic?
          setField adc_cr2_jexten adc_exten_rising
          setField adc_cr2_jextsel adc_jext_t1cc4

        -- FIXME: hardcoded channels
        comment "regular conversion sequence, sqr3 rank 1"
        modifyReg adcRegSQR3 $ setField adc_sqr3_sq1 (fromRep 5)

        comment "injected channel sequence, rank 1"
        modifyReg adcRegJSQR $ do
          --setField adc_jsq1 (fromRep 0)
          -- !!! only jsqr4 is read if jsqr_jl is 0, or not ???
          -- jsr and jdr correspondence? will better see this on another channel then 0
          setField adc_jsqr4 (fromRep 0)

        -- quality jsqr obfuscation
        -- _CHANNELNB_ << (5U * ((_RANKNB_ + 3U) - (_JSQR_JL_)))

        interrupt_enable int

    handler periodic "adc_periodic" $ do
      callback $ \_ -> do
        jdr1 <- getReg adcRegJDR1
        csr <- getReg adcRegCSR

        store lastadc (toRep (jdr1 #. adc_jdr1_data))
        store lastjeoc (toRep (csr #. adc_csr_jeoc1))

adc_in_pin :: GPIOPin -> Ivory eff ()
adc_in_pin p = do
  pinEnable  p
  pinSetMode p gpio_mode_analog

app :: (e -> ClockConfig)
    -> (e -> ADC)
    -> (e -> PWMOut)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc  totestadc totestpwm touart toleds = do
  towerDepends odriveTypes
  towerModule  odriveTypes

  adc  <- fmap totestadc getEnv
  pwm  <- fmap totestpwm getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv

  (buffered_ostream, _istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200

  monitor "dma" mon
  -- UART buffer transmits in buffers. We want to transmit byte-by-byte and let
  -- this monitor manage periodically flushing a buffer.
  ostream <- uartUnbuffer (buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))

  pwmTower pwm
  adcTower adc

  periodic <- period (Milliseconds 500)

  monitor "simplecontroller" $ do
    handler systemInit "init" $ do
      callback $ const $ do
        ledSetup $ redLED leds
        ledSetup $ blueLED leds

    handler periodic "periodic" $ do
      o <- emitter ostream 64
      callback $ \_ -> do
        puts o "q"
