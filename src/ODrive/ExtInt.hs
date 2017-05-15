{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module ODrive.ExtInt where

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW
import Ivory.Tower

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.Peripheral.SYSCFG
import Ivory.BSP.STM32.Peripheral.EXTI

import Ivory.BSP.STM32F405.SYSCFG
import Ivory.BSP.STM32F405.EXTI

import ODrive.Platforms

extIntTower :: ExtInt -> Tower e (ChanOutput ('Stored IBool))
extIntTower ExtInt {extInt=int, extPin=pin} = do
  let SYSCFG{..} = syscfg
  let EXTI{..} = exti

  chan <- channel

  debounce <- period (Milliseconds 100)

  isr <- signalUnsafe
            (Interrupt int)
            (Microseconds 250)
            (interrupt_disable int)

  monitor "extInt" $ do
    monitorModuleDef $ hw_moduledef

    int_cnt <- stateInit "int_cnt" (ival (0 :: Uint32))
    f_cnt <- stateInit "f_cnt" (ival (0 :: Uint32))
    debounceCnt <- stateInit "debounceCnt" (ival (0 :: Uint8))

    let line = fromIntegral $ gpioPinNumber pin
        ln = 1 `iShiftL` line

    handler isr "extint_handle" $ do
      e <- emitter (fst chan) 1
      callback $ const $ do
        dcnt <- deref debounceCnt
        when (dcnt ==? 0) $ do
          store debounceCnt 2
          emitV e true
          cnt <- deref f_cnt
          store f_cnt (cnt + 1)

        last_cnt <- deref int_cnt
        store int_cnt (last_cnt + 1)

        comment "clear interrupt flags in status register"
        modifyReg extiRegPR $ do
          setField exti_pr_data (fromRep ln)

        interrupt_enable int

    handler debounce "debounce" $ do
      callback $ const $ do
        cnt <- deref debounceCnt
        unless (cnt ==? 0) $ store debounceCnt (cnt - 1)

    handler systemInit "init" $ do
      callback $ const $ do
        syscfgRCCEnable
        extint_configure pin

        -- XXX: this needs driver as now we need to adjust for
        -- each extiN
        modifyReg syscfgRegEXTICR1 $ do
          setField syscfg_exticr1_exti2 (toEXTIPort pin)

        modifyReg extiRegIMR $ do
          setField exti_imr_data (fromRep ln)

        -- rising edge trigger
        --modifyReg extiRegRTSR $ do
          --setField exti_rtsr_data (fromRep ln)

        -- falling edge trigger
        modifyReg extiRegFTSR $ do
          setField exti_ftsr_data (fromRep ln)

        interrupt_enable int

  return (snd chan)

extint_configure :: GPIOPin -> Ivory eff ()
extint_configure p = do
  pinEnable   p
  pinSetMode  p gpio_mode_input
  pinSetPUPD  p gpio_pupd_pullup
  pinSetSpeed p gpio_speed_50mhz
