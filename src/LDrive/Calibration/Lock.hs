 {-# LANGUAGE DataKinds #-}
module LDrive.Calibration.Lock where

import Ivory.Language
import Ivory.Language.Cast
import Ivory.Stdlib
import Ivory.Tower

import LDrive.Types
import LDrive.Control.Modulation
import LDrive.Ivory.Types.Adc
import LDrive.Ivory.Types.CalR
import LDrive.Ivory.Types.Calibration

phaseLockTower :: IFloat
               -> ChanOutput ('Struct "calibration")
               -> PWMInput
               -> Tower e (
                      ChanInput ('Struct "adc")
                    , ChanOutput ('Struct "calibration"))
phaseLockTower measHz calOut timingsIn = do
  adcChan <- channel
  calResult <- channel

  monitor "cal_phase_lock" $ do
    pwmout <- stateInit "pwmout" $ iarray [ival 0, ival 0, ival 0]
    cyclesElapsed <- stateInit "cyclesElapsed" $ ival (0 :: Uint16)

    calib <- state "calib"

    handler calOut "cal_phase_lock" $ do
      callback $ refCopy calib

    handler (snd adcChan) "adc_phase_lock" $ do
      timings <- emitter timingsIn 1
      result <- emitter (fst calResult) 1
      callback $ \adc -> do
        vm <- calib ~>* voltageMag
        bus <- adc ~>* vbus
        voltage_modulation bus vm 0.0 pwmout
        emit timings (constRef pwmout)

        cycles <- deref cyclesElapsed
        when (cycles >=? castDefault measHz) $ do
          emit result $ constRef calib

        cyclesElapsed %= (+1)

  return (fst adcChan, snd calResult)
