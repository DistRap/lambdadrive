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

[ivory|

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
|]
