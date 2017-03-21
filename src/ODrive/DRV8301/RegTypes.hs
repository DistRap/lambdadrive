{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ODrive.DRV8301.RegTypes where

import Ivory.Language

[ivory|
 bitdata DrvRW :: Bit
   = drv_read  as 0b1
   | drv_write  as 0b0

 bitdata DrvAddr :: Bits 4
   = drv_status1  as 0b0000
   | drv_status2  as 0b0001
   | drv_control1 as 0b0010
   | drv_control2 as 0b0011

-- CTRL1

 bitdata GateCurrent :: Bits 2
   = drv_gate_17 as 0b00
   | drv_gate_07 as 0b01
   | drv_gate_025 as 0b10
 --  | reserved as 0b11

 bitdata GateReset :: Bit
   = drv_gate_reset_normal  as 0b0
   | drv_gate_reset_latched as 0b1

 bitdata PWMMode :: Bit
   = drv_pwm6 as 0b0
   | drv_pwm3 as 0b1

 bitdata OCMode :: Bits 2
   = drv_ocmode_current_limit as 0b00
   | drv_ocmode_latch_shutdown as 0b01
   | drv_ocmode_report_only as 0b10
   | drv_ocmode_disabled as 0b11

 bitdata OCAdj :: Bits 5
  = drv_ocadj_0p060 as  0
  | drv_ocadj_0p068 as  1
  | drv_ocadj_0p076 as  2
  | drv_ocadj_0p086 as  3
  | drv_ocadj_0p097 as  4
  | drv_ocadj_0p109 as  5
  | drv_ocadj_0p123 as  6
  | drv_ocadj_0p138 as  7
  | drv_ocadj_0p155 as  8
  | drv_ocadj_0p175 as  9
  | drv_ocadj_0p197 as 10
  | drv_ocadj_0p222 as 11
  | drv_ocadj_0p250 as 12
  | drv_ocadj_0p282 as 13
  | drv_ocadj_0p317 as 14
  | drv_ocadj_0p358 as 15
  | drv_ocadj_0p403 as 16
  | drv_ocadj_0p454 as 17
  | drv_ocadj_0p511 as 18
  | drv_ocadj_0p576 as 19
  | drv_ocadj_0p648 as 20
  | drv_ocadj_0p730 as 21
  | drv_ocadj_0p822 as 22
  | drv_ocadj_0p926 as 23
  | drv_ocadj_1p043 as 24
  | drv_ocadj_1p175 as 25
  | drv_ocadj_1p324 as 26
  | drv_ocadj_1p491 as 27
  | drv_ocadj_1p679 as 28
  | drv_ocadj_1p892 as 29
  | drv_ocadj_2p131 as 30
  | drv_ocadj_2p400 as 31

-- CTRL2

 bitdata OCTWMode :: Bits 2
  = drv_octw_both    as 0b00
  | drv_octw_ot_only as 0b01
  | drv_octw_oc_only as 0b10
  -- reserverd as 0b11

 -- shunt amp gain in [V per V]
 bitdata Gain :: Bits 2
  = drv_gain_10  as 0b00
  | drv_gain_20  as 0b01
  | drv_gain_40  as 0b10
  | drv_gain_80  as 0b11

 bitdata OCToff :: Bit
   = drv_oc_toff_cycle_by_cycle   as 0b0
   | drv_oc_toff_off_time_control as 0b1

|]
