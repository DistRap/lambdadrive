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

module ODrive.Encoder where

import Ivory.Language
import Ivory.HW
import Ivory.Tower
import ODrive.Platforms

import Ivory.BSP.STM32.Peripheral.GTIM2345
import Ivory.BSP.STM32.Peripheral.GPIOF4

data Encoder =
  Encoder
    { encoder_get_count :: forall eff . Ivory eff Uint16
    , encoder_get_dir   :: forall eff . Ivory eff IBool
    }

encoderTower :: Enc -> Tower e Encoder
encoderTower (EncTimer {encTim=tim@GTIM {..}, encChan1=c1, encChan2=c2, encAf=af}) = do
  periodic <- period (Milliseconds 500)

  monitor "encoder_capture" $ do
    monitorModuleDef $ hw_moduledef

    count <- stateInit "count" (ival (0 :: Uint16))
    dir <- state "dir" -- IBool

    handler systemInit "init" $ do
      callback $ const $ do
        encoderInitPin c1 af
        encoderInitPin c2 af
        encoderInitTimer tim

    handler periodic "periodic" $ do
      callback $ \_ -> do
        cnt <- encoderGetCount tim
        store count cnt

        d <- encoderGetDir tim
        store dir d

  return $ Encoder (encoderGetCount tim) (encoderGetDir tim)

encoderGetCount :: GTIM GTIM_16 -> Ivory eff Uint16
encoderGetCount GTIM{..} = do
  cnt <- getReg gtimRegCNT
  return $ toRep (cnt #. gtim_16_data)

encoderGetDir :: GTIM t -> Ivory eff IBool
encoderGetDir GTIM{..} = do
  d <- getReg gtimRegCR1
  return $ bitToBool (d #. gtim_cr1_dir)

encoderInit :: Enc -> Ivory eff ()
encoderInit (EncTimer {..}) = do
  mapM_ (flip encoderInitPin encAf) [encChan1, encChan2]
  encoderInitTimer encTim

encoderInitPin :: GPIOPin -> GPIO_AF -> Ivory eff ()
encoderInitPin p af = do
  pinEnable p
  pinSetAF  p af
  pinSetPUPD p gpio_pupd_none
  pinSetMode p gpio_mode_af

encoderInitTimer :: GTIM GTIM_16 -> Ivory eff ()
encoderInitTimer GTIM {..} = do
  gtimRCCEnable
  -- XXX: clearing not needed
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
