{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ODrive.Tests.ADCMulti where

import Data.Char (ord)

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
import ODrive.Tests.PWM hiding (uartTestTypes)
import BSP.Tests.UART.Buffer
import BSP.Tests.UART.Types

-- ADC1 vbus
-- ADC2 phase B
-- ADC2 phase C

adcMultiTower (
    ADC {adcPeriph=adcp1, adcChan=chan1, adcInjChan=ichan1, adcInt=int}
  , ADC {adcPeriph=adcp2, adcChan=chan2, adcInjChan=ichan2, adcInt=_}
  , ADC {adcPeriph=adcp3, adcChan=chan3, adcInjChan=ichan3, adcInt=_}) = do

  periodic <- period (Milliseconds 500)

-- FIXME
-- +#define  TICK_INT_PRIORITY            ((uint32_t)15U)   /*!< tick interrupt priority */

  isr <- signalUnsafe
            (Interrupt int)
            (Microseconds 250)
            (interrupt_disable int)

  monitor "adc_multi" $ do
    monitorModuleDef $ hw_moduledef

    lastadc <- stateInit "lastadc" (ival (0 :: Uint16))
    lastadci <- stateInit "lastadci" (ival (0 :: Uint16))
    lastjeoc <- stateInit "lastjeoc" (ival (0 :: Uint8))

    adc_last_regular <- stateInit "adc_last_regular" (ival (0 :: Uint16))
    adc_last_injected <- stateInit "adc_last_injected" (ival (0 :: Uint16))

    -- isr source
    -- adc_src_none M1 dccal
    -- adc_src_t1cc4 M0 c
    -- adc_src_t1trgo M0 dccal
    -- adc_src_t8cc4 M1 c

    vbus <- stateInit "vbus" (ival (0 :: Uint16))
    phase_b <- stateInit "phase_b" (ival (0 :: Uint16))
    phase_c <- stateInit "phase_c" (ival (0 :: Uint16))

    tmp <- stateInit "tmp" (ival (0 :: Uint8))

    handler isr "adc_capture" $ do
      callback $ const $ do

        -- TODO: get and store all ADCs
        -- state machine
        -- git show 3b268eed6a5396f5ac5439e1f2a8144cfccc86a7
        -- current and dc cal isrs should arrive in order ADC2 -> ADC3
        -- both phases are read at the same time
        --
        -- if source is CC4 we're measuring current at SVM vector 0
        -- if source is TRGO we're measuring DC_CAL at SVM vector 7
        --
        -- TODO: add adcId to record and use instead of i, pass ADCPeriph to handleADC as well

        -- handle adc reading from ADC`i`
        let handleADC i isInjected = do
              val <- deref adc_last_injected

              -- XXX: we only handle injected conversions for now
              cond_
                [ i ==? 1 ==> store (vbus) val
                -- XXX: adc2/3 should switch from T1_CC4 to T1_TRGO and vice versa
                -- adc2 in13
                , i ==? 2 ==> store (phase_b) val
                , i ==? 3 ==> store (phase_c) val
                ]

        -- check which ADC fired
        let checkADC ADCPeriph{..} i = do
              sr <- getReg adcRegSR

              comment "regular conversion"
              when (bitToBool (sr #. adc_sr_eoc)) $ do
                dr <- getReg adcRegDR

                store adc_last_regular (toRep (dr #. adc_dr_data))

                handleADC i false

                modifyReg adcRegSR $ do
                  clearBit adc_sr_strt
                  clearBit adc_sr_eoc

              comment "injected conversion"
              when (bitToBool (sr #. adc_sr_jeoc)) $ do
                jdr1 <- getReg adcRegJDR1

                store adc_last_injected (toRep (jdr1 #. adc_jdr1_data))

                handleADC i true

                comment "clear interrupt flags in status register"
                modifyReg adcRegSR $ do
                  clearBit adc_sr_jstrt
                  clearBit adc_sr_jeoc

        mapM_ (uncurry checkADC) (zip [adcp1, adcp2, adcp3] [1 :: Uint8, 2, 3])

        interrupt_enable int

    handler systemInit "init" $ do
      callback $ const $ do
        mapM_ adc_in_pin $ map snd [chan1, chan2, chan3, ichan1, ichan2, ichan3]

        adcInit adcp1 adc_12bit false
        adcInit adcp2 adc_12bit false
        adcInit adcp3 adc_12bit false

        let adcSetup ADCPeriph{..}
                     exten extsel regChan
                     jexten jextsel injChan = do

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
                setField adc_cr2_exten exten
                setField adc_cr2_extsel extsel

                setField adc_cr2_jexten jexten
                setField adc_cr2_jextsel jextsel

              comment "regular conversion sequence, sqr3 rank 1"
              modifyReg adcRegSQR3 $ setField adc_sqr3_sq1 (fromRep regChan)

              comment "injected channel sequence, rank 1"
              modifyReg adcRegJSQR $ do
                --setField adc_jsq1 (fromRep 0)
                -- !!! only jsqr4 is read if jsqr_jl is 0, or not ???
                -- jsr and jdr correspondence? will better see this on another channel then 0
                setField adc_jsqr4 (fromRep injChan)

        -- quality jsqr obfuscation
        -- _CHANNELNB_ << (5U * ((_RANKNB_ + 3U) - (_JSQR_JL_)))
        adcSetup adcp1
          adc_exten_none adc_ext_t8trgo   (fst chan1)
          adc_exten_rising adc_jext_t1cc4 (fst ichan1)

        --store tmp (fst ichan2)
        adcSetup adcp2
          adc_exten_none adc_ext_t8trgo (fst chan2)
          adc_exten_rising adc_jext_t1cc4 (fst ichan2)

        adcSetup adcp3
          adc_exten_rising adc_ext_t8trgo (fst chan3)
          adc_exten_rising adc_jext_t1cc4 (fst ichan3)

        interrupt_enable int

adc_in_pin :: GPIOPin -> Ivory eff ()
adc_in_pin p = do
  pinEnable  p
  pinSetMode p gpio_mode_analog

-- /end of relevant stuff
-- generic test app bones following, move this to lib and test lib

app :: (e -> ClockConfig)
    -> (e -> ADCs)
    -> (e -> PWM)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc  totestadcs totestpwm touart toleds = do
  towerDepends uartTestTypes
  towerModule  uartTestTypes

  adcs <- fmap totestadcs getEnv
  pwm  <- fmap totestpwm getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv

  (buffered_ostream, istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200

  monitor "dma" mon
  -- UART buffer transmits in buffers. We want to transmit byte-by-byte and let
  -- this monitor manage periodically flushing a buffer.
  ostream <- uartUnbuffer (buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))

  pwmTower pwm
  adcMultiTower adcs

  periodic <- period (Milliseconds 500)

  monitor "simplecontroller" $ do
    write <- stateInit "write" (ival true)
    handler systemInit "init" $ do
      callback $ const $ do
        ledSetup $ redLED leds
        ledSetup $ blueLED leds

    handler periodic "periodic" $ do
      o <- emitter ostream 64
      callback $ \_ -> do
        puts o "q"

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral $ ord c)

uartTestTypes :: Module
uartTestTypes = package "uartTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)
