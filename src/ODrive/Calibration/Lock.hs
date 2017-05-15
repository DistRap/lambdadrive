 {-# LANGUAGE DataKinds #-}
module ODrive.Calibration.Lock where

import Ivory.Language
import Ivory.Language.Cast
import Ivory.Stdlib
import Ivory.Tower

import ODrive.Types
import ODrive.Control.Modulation
import ODrive.Ivory.Types.Adc
import ODrive.Ivory.Types.CalR
import ODrive.Ivory.Types.Calibration

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
