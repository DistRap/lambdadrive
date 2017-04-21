{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module ODrive.PWM where

import Ivory.Language
import Ivory.HW
import Ivory.Tower
import ODrive.Platforms

import Ivory.BSP.STM32.Peripheral.ATIM18
import Ivory.BSP.STM32.Peripheral.GPIOF4

data PWM =
  PWM
    { pwm_set :: forall s cs eff .  (GetAlloc eff ~ 'Scope s) => ConstRef cs ('Array 3 ('Stored Uint16)) -> Ivory eff ()
    }

pwmTower :: PWMOut -> Tower e PWM
pwmTower (PWMTimer {pwmTim=atim@ATIM {..},
                    pwmC1=c1, pwmC2=c2, pwmC3=c3,
                    pwmC1N=c1n, pwmC2N=c2n, pwmC3N=c3n,
                    pwmAf=af, pwmInit=initPWM}) = do

  monitor "pwm" $ do
    monitorModuleDef $ hw_moduledef

    handler systemInit "init" $ do
      callback $ const $ do
        atimRCCEnable

        mapM_ (pwm_out_pin af) [c1, c2, c3, c1n, c2n, c3n]

        comment "time counter mode up"
        modifyReg atimRegCR1 $ do
          setField atim_cr1_dir cr1_dir_up
          setField atim_cr1_cms cr1_cms_center_align3

        comment "output compare pwm mode 2"
        modifyReg atimRegCCMR1_OCM $ do
          setField atim_ccmr1_ocm_oc1m ccmr_mode_pwm2
          setField atim_ccmr1_ocm_oc2m ccmr_mode_pwm2
          setBit atim_ccmr1_ocm_oc1pe
          setBit atim_ccmr1_ocm_oc2pe

        comment "output compare pwm mode 2"
        modifyReg atimRegCCMR2_OCM $ do
          setField atim_ccmr2_ocm_oc3m ccmr_mode_pwm2
          setField atim_ccmr2_ocm_oc4m ccmr_mode_pwm2
          setBit atim_ccmr2_ocm_oc3pe
          setBit atim_ccmr2_ocm_oc4pe

        comment "master output trigger update (TRGO)"
        modifyReg atimRegCR2 $ setField atim_cr2_mms cr2_mms_update

        comment "set reload value"
        -- should be part of PWMTimer record
        modifyReg atimRegARR $ setField atim_16_data (fromRep tim_period_clocks)

        comment "enable outputs and complementary outputs"
        modifyReg atimRegCCER $ do
          setBit atim_ccer_cc1e
          setBit atim_ccer_cc2e
          setBit atim_ccer_cc3e
          setBit atim_ccer_cc4e

          setBit atim_ccer_cc1ne
          setBit atim_ccer_cc2ne
          setBit atim_ccer_cc3ne

        comment "enable timer periph"
        modifyReg atimRegCR1 $ do
          setBit atim_cr1_cen

        comment "enable main output"
        comment "enable off state run, off state idle"
        comment "deadtime to 20, break active high"
        modifyReg atimRegBDTR $ do
          setBit atim_bdtr_moe
          setBit atim_bdtr_ossr
          setBit atim_bdtr_ossi
          -- #define TIM_1_8_DEADTIME_CLOCKS 20
          -- we should be able to compute this but
          -- datasheet is a bit cryptic (page 586)

          -- seems to be about 120ns
          setField atim_bdtr_dtg (fromRep 20)
          setBit atim_bdtr_bkp

        comment "channel 4 ccr to 1"
        modifyReg atimRegCCR4 $ setField atim_16_data (fromRep 0x1)

        initpwm <- local (iarray [ ival initPWM, ival initPWM, ival initPWM])
        pwm_set_atim atim (constRef initpwm)

        -- * Generate an update event to reload the prescaler
        -- and the repetition counter(only for TIM1 and TIM8) value immediately
        -- (To avoid wasted time cycles)
        -- TIMx->EGR = TIM_EGR_UG;
        --
  return $ PWM (pwm_set_atim atim)

pwm_out_pin :: GPIO_AF -> GPIOPin -> Ivory eff ()
pwm_out_pin af p = do
  pinEnable   p
  pinSetMode  p gpio_mode_af
  pinSetPUPD  p gpio_pupd_none
  pinSetSpeed p gpio_speed_50mhz
  pinSetAF    p af

pwm_set_atim :: ATIM -> ConstRef s ('Array 3 ('Stored Uint16)) -> Ivory eff ()
pwm_set_atim atim vals = do
  set_ccr (atimRegCCR1 atim) (vals ! 0)
  set_ccr (atimRegCCR2 atim) (vals ! 1)
  set_ccr (atimRegCCR3 atim) (vals ! 2)
  where
  set_ccr reg val = do
    c <- deref val
    setReg reg (setField atim_16_data (fromRep c))
