{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module ODrive.Tests.Encoder where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Peripheral.GTIM2345
import Ivory.BSP.STM32.Peripheral.GPIOF4

import ODrive.Platforms
import ODrive.LED
import ODrive.Types
import ODrive.Utils

encoderTower (EncTimer {encTim=GTIM {..},  encChan1=c1, encChan2=c2, encAf=af}) = do

  periodic <- period (Milliseconds 500)

  monitor "encoder_capture" $ do
    monitorModuleDef $ hw_moduledef

    count <- stateInit "count" (ival (0 :: Uint16))
    dir <- stateInit "dir" (ival (0 :: Uint8))

    handler systemInit "init" $ do
      callback $ const $ do
        gtimRCCEnable

        pinEnable c1
        pinEnable c2

        pinSetAF c1 af
        pinSetAF c2 af

        pinSetPUPD c1 gpio_pupd_none
        pinSetPUPD c2 gpio_pupd_none

        pinSetMode c1 gpio_mode_af
        pinSetMode c2 gpio_mode_af

        -- FIXME: clearing not needed
        comment "reset smcr sms bits"
        modifyReg gtimRegSMCR $ setField gtim_smcr_sms (fromRep 0)

        comment "time counter mode up"
        modifyReg gtimRegCR1 $ do
          clearBit gtim_cr1_dir
          setField gtim_cr1_cms (fromRep 0)

          -- divider to 0 (div1)
          setField gtim_cr1_ckd (fromRep 0)

        comment "clear prescale"
        modifyReg gtimRegPSC $ setField gtim_psc_psc (fromRep 0)

        comment "reload value to max val"
        modifyReg gtimRegARR $ setField gtim_16_data (fromRep 0xFFFF)

        comment "encoder mode 3"
        modifyReg gtimRegSMCR $ setField gtim_smcr_sms sms_mode_enc3

        comment "input capture selection"
        modifyReg gtimRegCCMR1_ICM $ do
          setField gtim_ccmr1_icm_cc1s ccs_mode_in1
          setField gtim_ccmr1_icm_cc2s ccs_mode_in1

          -- prescaler
          setField gtim_ccmr1_icm_ic1psc (fromRep 0)
          setField gtim_ccmr1_icm_ic2psc (fromRep 0)

          -- filter
          setField gtim_ccmr1_icm_ic1f (fromRep 4)
          setField gtim_ccmr1_icm_ic2f (fromRep 4)

        comment "enable channels"
        modifyReg gtimRegCCER $ do
          setBit gtim_ccer_cc1e
          setBit gtim_ccer_cc2e

        comment "enable timer periph"
        modifyReg gtimRegCR1 $ do
          setBit gtim_cr1_cen

    handler periodic "periodic" $ do
      callback $ \_ -> do
        cnt <- getReg gtimRegCNT
        store count $ toRep (cnt #. gtim_16_data)

        d <- getReg gtimRegCR1
        store dir $ toRep (d #. gtim_cr1_dir)

app :: (e -> ClockConfig)
    -> (e -> Enc)
    -> (e -> TestUART)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestenc touart toleds = do
  towerDepends odriveTypes
  towerModule  odriveTypes

  enc  <- fmap totestenc getEnv
  leds <- fmap toleds getEnv
  uart <- fmap touart getEnv

  (buffered_ostream, istream, mon) <- uartTower tocc (testUARTPeriph uart) (testUARTPins uart) 115200

  monitor "dma" mon
  -- UART buffer transmits in buffers. We want to transmit byte-by-byte and let
  -- this monitor manage periodically flushing a buffer.
  ostream <- uartUnbuffer (buffered_ostream :: BackpressureTransmit UARTBuffer ('Stored IBool))

  encoderTower enc

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
