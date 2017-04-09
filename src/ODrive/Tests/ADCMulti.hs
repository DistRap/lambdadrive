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
import Ivory.BSP.STM32.Peripheral.GPIOF4

import ODrive.Platforms
import ODrive.LED
import ODrive.Tests.PWM hiding (uartTestTypes)
import BSP.Tests.UART.Buffer
import BSP.Tests.UART.Types

[ivory|
struct adc_sample
  { vbus    :: Stored Uint16
  ; phase_a :: Stored Uint16
  ; phase_b :: Stored Uint16
  ; phase_c :: Stored Uint16
  ; meas_t  :: Stored ITime
  }
|]

adc_types :: Module
adc_types = package "adc_types" $ do
  defStruct (Proxy :: Proxy "adc_sample")


setJExt :: ADCPeriph -> ADCJExtSel -> Ivory eff ()
setJExt ADCPeriph{..} jextsel = modifyReg adcRegCR2 $ setField adc_cr2_jextsel jextsel

-- ADC1 vbus
-- ADC2 phase B
-- ADC3 phase C

adcMultiTower :: (ADC, ADC, ADC)
              -> Tower e (ChanOutput ('Struct "adc_sample"))
adcMultiTower (
    a1@ADC {adcPeriph=adcp1, adcChan=chan1, adcInjChan=ichan1, adcInt=int}
  , a2@ADC {adcPeriph=adcp2, adcChan=chan2, adcInjChan=ichan2, adcInt=_}
  , a3@ADC {adcPeriph=adcp3, adcChan=chan3, adcInjChan=ichan3, adcInt=_}) = do


  towerModule adc_types
  towerDepends adc_types

  periodic <- period (Milliseconds 500)

  adc_chan <- channel

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

    adc_vbus    <- stateInit "adc_vbus" (ival (0 :: Uint16))
    adc_phase_b <- stateInit "adc_phase_b" (ival (0 :: Uint16))
    adc_phase_c <- stateInit "adc_phase_c" (ival (0 :: Uint16))

    tmp <- stateInit "tmp" (ival (0 :: Uint8))

    handler isr "adc_capture" $ do
      e <- emitter (fst adc_chan) 1
      callback $ const $ do

        -- current and dc cal isrs should arrive in order ADC2 -> ADC3
        -- both phases are read at the same time
        --
        -- if source is CC4 we're measuring current at SVM vector 0
        -- if source is TRGO we're measuring DC_CAL at SVM vector 7
        --
        -- populate adc_meas struct with current values
        let mkMeas :: forall s eff . (GetAlloc eff ~ 'Scope s) => Ivory eff (ConstRef ('Stack s) ('Struct "adc_sample"))
            mkMeas = do
              pb <- deref adc_phase_b
              pc <- deref adc_phase_c

              t <- getTime

              meas <- local $ istruct
                [ phase_a .= ival 0
                , phase_b .= ival pb
                , phase_c .= ival pc ]

              store (meas ~> meas_t) t
              --  , meas_t  .= t ]

              return $ constRef meas

        -- handle adc reading from ADC, toggle between t1cc4 and t1trgo
        let handleADC ADC{..} isInjected = do
              val <- deref adc_last_injected

              cr2 <- getReg $ adcRegCR2 adcPeriph
              let trgSrc = cr2 #. adc_cr2_jextsel
              let trgSrcT1CC4 = trgSrc ==? adc_jext_t1cc4
              let trgSrcT1TRGO = trgSrc ==? adc_jext_t1trgo
              let i = adcId

              comment "flip between t1cc4 and t1trgo for adc2 and 3"
              when (i ==? 2 .|| i ==? 3) $ do
                cond_
                  [ trgSrcT1CC4  ==> setJExt adcPeriph adc_jext_t1trgo
                  , trgSrcT1TRGO ==> setJExt adcPeriph adc_jext_t1cc4 ]

              -- XXX: we only handle injected conversions for now
              cond_
                [ (i ==? 1) ==> do
                    store (adc_vbus) val

                , (i ==? 2) ==> do
                    store (adc_phase_b) val

                , (i ==? 3) ==> do
                    store (adc_phase_c) val
                    meas <- mkMeas
                    emit e meas
                ]

        -- check which ADC fired
        let checkADC a@ADC{..} = do
              let ADCPeriph{..} = adcPeriph

              sr <- getReg adcRegSR

              comment "regular conversion"
              when (bitToBool (sr #. adc_sr_eoc)) $ do
                dr <- getReg adcRegDR

                store adc_last_regular (toRep (dr #. adc_dr_data))

                handleADC a false

                modifyReg adcRegSR $ do
                  clearBit adc_sr_strt
                  clearBit adc_sr_eoc

              comment "injected conversion"
              when (bitToBool (sr #. adc_sr_jeoc)) $ do
                jdr1 <- getReg adcRegJDR1

                store adc_last_injected (toRep (jdr1 #. adc_jdr1_data))

                handleADC a true

                comment "clear interrupt flags in status register"
                modifyReg adcRegSR $ do
                  clearBit adc_sr_jstrt
                  clearBit adc_sr_jeoc

        mapM_ checkADC [a1, a2, a3]

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

  -- return output side of the channel
  return (snd adc_chan)

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

  adc_chan <- adcMultiTower adcs

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

    ladc_a <- stateInit "ladc_a" (ival (0 :: Uint16))
    ladc_b <- stateInit "ladc_b" (ival (0 :: Uint16))
    ladc_c <- stateInit "ladc_c" (ival (0 :: Uint16))
    ladc_tim <- stateInit "ladc_tim" (ival (0 :: ITime))
    msgCount <- stateInit "msgcount" (ival (0 :: Uint64))

    handler adc_chan "adc_chan" $ do
      callback $ \x -> do
        a <- (x ~>* phase_a)
        b <- (x ~>* phase_b)
        c <- (x ~>* phase_c)
        t <- (x ~>* meas_t)
        store ladc_a a
        store ladc_b b
        store ladc_c c
        store ladc_tim t

        c <- deref msgCount
        store msgCount (c+1)

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral $ ord c)

uartTestTypes :: Module
uartTestTypes = package "uartTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)
