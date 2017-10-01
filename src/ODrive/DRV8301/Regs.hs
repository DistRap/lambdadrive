{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ODrive.DRV8301.Regs where

import Ivory.Language
import ODrive.DRV8301.RegTypes
import LDrive.Ivory.Types.DrvFault

[ivory|
 bitdata Drv8301 :: Bits 16 = drv_dat_reg
   { drv_rw :: DrvRW
   , drv_addr :: DrvAddr
   , drv_data :: Bits 11 }

 bitdata CTRL1 :: Bits 11 = ctrl1
  { drv_ocadj :: OCAdj
  , drv_ocmode :: OCMode
  , drv_pwmmode :: PWMMode
  , drv_gate_reset :: GateReset
  , drv_gate :: GateCurrent
  }

  bitdata CTRL2 :: Bits 11 = ctrl2
   { drv_reserved :: Bits 4
   , drv_oc_toff  :: OCToff
   , drv_dc_cal_ch2 :: Bit
   , drv_dc_cal_ch1 :: Bit
   , drv_gain :: Gain
   , drv_octw :: OCTWMode
   }

  bitdata STATUS1 :: Bits 11 = status1
   { drv_sta1_fault    :: Bit
   , drv_sta1_gvdd_uv  :: Bit
   , drv_sta1_pvdd_uv  :: Bit
   , drv_sta1_otsd     :: Bit -- overtemperature shutdown
   , drv_sta1_otw      :: Bit -- overtemperature warning
   , drv_sta1_fetha_oc :: Bit
   , drv_sta1_fetla_oc :: Bit
   , drv_sta1_fethb_oc :: Bit
   , drv_sta1_fetlb_oc :: Bit
   , drv_sta1_fethc_oc :: Bit
   , drv_sta1_fetlc_oc :: Bit
   }

  bitdata STATUS2 :: Bits 11 = status2
   { _                :: Bits 3
   , drv_sta2_gvdd_ov :: Bit
   , _                :: Bits 3
   , drv_sta2_dev_id  :: Bits 4
   }
|]


drvFaultFromRegs :: Uint16 -> Uint16 -> Ref s ('Struct "drv_fault") -> Ivory eff ()
drvFaultFromRegs s1 s2 r = do
  p1 fault              drv_sta1_fault
  p1 gvdd_undervoltage  drv_sta1_gvdd_uv
  p2 gvdd_overvoltage   drv_sta2_gvdd_ov
  p1 pvdd_undervoltage  drv_sta1_pvdd_uv
  p1 overtemp_shutdown  drv_sta1_otsd
  p1 overtemp_warning   drv_sta1_otw

  p1 fet_hi_a_overcurr  drv_sta1_fetha_oc
  p1 fet_lo_a_overcurr  drv_sta1_fetla_oc

  p1 fet_hi_b_overcurr  drv_sta1_fethb_oc
  p1 fet_lo_b_overcurr  drv_sta1_fetlb_oc

  p1 fet_hi_c_overcurr  drv_sta1_fethc_oc
  p1 fet_lo_c_overcurr  drv_sta1_fetlc_oc
  where
    p1 lbl field = store (r ~> lbl) (bitToBool (fromRep s1 #. field))
    p2 lbl field = store (r ~> lbl) (bitToBool (fromRep s2 #. field))
