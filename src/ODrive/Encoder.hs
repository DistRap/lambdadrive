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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ODrive.Encoder where

import Ivory.Language
import Ivory.HW
import Ivory.Tower
import Ivory.Language.Cast
import ODrive.Platforms
import ODrive.Types
import ODrive.Ivory.Types.Encoder

import Ivory.BSP.STM32.Peripheral.GTIM2345
import Ivory.BSP.STM32.Peripheral.GPIOF4

data Encoder =
  Encoder
    { encoder_get_count :: forall eff . Ivory eff Uint16
    , encoder_get_dir   :: forall eff . Ivory eff IBool
    , encoder_get       :: forall s s1 eff . (GetAlloc eff ~ 'Scope s) =>
                           Ref s1 ('Struct "encoder_state")
                           -> Ivory eff (ConstRef ('Stack s) ('Struct "encoder"))
    }

[ivory|
struct encoder_state
  { enc_count      :: Stored Sint32
  ; enc_cpr        :: Stored Sint32
  ; enc_offset     :: Stored IFloat
  ; enc_poles      :: Stored Uint8
  ; enc_pll_period :: Stored IFloat
  ; enc_pll_kp     :: Stored IFloat
  ; enc_pll_ki     :: Stored IFloat
  ; enc_pll_pos    :: Stored IFloat
  ; enc_pll_vel    :: Stored IFloat
  }
|]

encoderTypes :: Module
encoderTypes = package "encoder_state_types" $ do
  defStruct (Proxy :: Proxy "encoder_state")

encoderTower :: Enc -> Tower e Encoder
encoderTower (EncTimer {encTim=tim@GTIM {..}, encChan1=c1, encChan2=c2, encAf=af}) = do
  periodic <- period (Milliseconds 500)

  towerDepends encoderTypes
  towerModule  encoderTypes

  monitor "encoder_capture" $ do
    monitorModuleDef $ hw_moduledef

    enccount <- stateInit "count" (ival (0 :: Uint16))
    encdir <- state "dir" -- IBool

    handler systemInit "init" $ do
      callback $ const $ do
        encoderInitPin c1 af
        encoderInitPin c2 af
        encoderInitTimer tim

    handler periodic "periodic" $ do
      callback $ \_ -> do
        cnt <- encoderGetCount tim
        store enccount cnt

        d <- encoderGetDir tim
        store encdir d

  return $ Encoder (encoderGetCount tim) (encoderGetDir tim) (encoderGet tim)

encoderGetCount :: GTIM GTIM_16 -> Ivory eff Uint16
encoderGetCount GTIM{..} = do
  cnt <- getReg gtimRegCNT
  return $ toRep (cnt #. gtim_16_data)

encoderGetDir :: GTIM t -> Ivory eff IBool
encoderGetDir GTIM{..} = do
  d <- getReg gtimRegCR1
  return $ bitToBool (d #. gtim_cr1_dir)

encoderGet :: GetAlloc eff ~ 'Scope s
           => GTIM GTIM_16
           -> Ref s1 ('Struct "encoder_state")
           -> Ivory eff (ConstRef ('Stack s) ('Struct "encoder"))
encoderGet gtim encState = do
  encCurrentCount <- encoderGetCount gtim
  encCurrentDir <- encoderGetDir gtim

  encCount <- deref (encState ~> enc_count)
  encCpr <- deref (encState ~> enc_cpr)
  encOffset <- deref (encState ~> enc_offset)
  poles <- fmap safeCast $ deref (encState ~> enc_poles)

   -- XXX: trickery from Sint32 -> Sint16
   -- we look at bottom 16 bits only as the hardware counter is also
   -- 32bit, this is exploited to get delta
  let delta :: Sint16
      delta = (twosComplementCast encCurrentCount) - (ivoryCast :: Sint32 -> Sint16) encCount
      newstate :: Sint32
      newstate = safeCast $ encCount + (safeCast delta)

  store (encState ~> enc_count) newstate

  let ph :: IFloat
      ph = safeCast (newstate .% encCpr) - encOffset

      elecRadPerEnc :: IFloat
      elecRadPerEnc = poles * 2 * pi * (1/(safeCast encCpr)) :: IFloat

      rotorPhase :: IFloat
      rotorPhase = (elecRadPerEnc * ph) .% (2*pi)

  comment "PLL"
  pllKp <- deref (encState ~> enc_pll_kp)
  pllKi <- deref (encState ~> enc_pll_ki)
  pllPeriod <- deref (encState ~> enc_pll_period)

  comment "predict current position"
  pllpos <- deref (encState ~> enc_pll_pos)
  pllvel <- deref (encState ~> enc_pll_vel)

  let newpllpos = pllpos + pllPeriod * pllvel

  comment "discrete phase detector"
  let deltaPos :: IFloat
      deltaPos = safeCast $ newstate - (castWith 0 $ floorF newpllpos)

  comment "pll feedback"
  let feedPllPos = newpllpos + pllPeriod * pllKp * deltaPos
      feedPllVel = pllvel + pllPeriod * pllKi * deltaPos
  store (encState ~> enc_pll_pos) feedPllPos
  store (encState ~> enc_pll_vel) feedPllVel

  sample <- local $ istruct
    [ count .= ival newstate
    , dir .= ival encCurrentDir
    , phase .= ival rotorPhase
    , pll_pos .= ival feedPllPos
    , pll_vel .= ival feedPllVel
    ]

  return $ constRef sample

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
