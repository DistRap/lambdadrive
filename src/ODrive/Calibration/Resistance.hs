{-# LANGUAGE DataKinds #-}
module ODrive.Calibration.Resistance where

import Ivory.Language
import Ivory.Language.Cast
import Ivory.Stdlib
import Ivory.Tower

import ODrive.Types
import ODrive.Control.Modulation
import ODrive.Ivory.Types.Adc
import ODrive.Ivory.Types.CalR
import ODrive.Ivory.Types.Calibration

phaseResistanceTower :: IFloat
                     -> ChanOutput ('Struct "calibration")
                     -> PWMInput
                     -> Tower e (
                            ChanInput ('Struct "adc")
                          , ChanOutput ('Struct "calibration"))
phaseResistanceTower measPeriod calOut timingsIn = do
  -- XXX: depends on fconstrain
  adcChan <- channel
  calResult <- channel

  monitor "cal_phase_resistance" $ do
    pwmout <- stateInit "pwmout" $ iarray [ival 0, ival 0, ival 0]
    testVoltage <- stateInit "testVoltage" $ ival 0
    cyclesElapsed <- stateInit "cyclesElapsed" $ ival (0 :: Uint16)

    calib <- state "calib"

    handler calOut "cal_phase_resistance" $ do
      callback $ refCopy calib

    handler (snd adcChan) "adc_phase_resistance" $ do
      timings <- emitter timingsIn 1
      result <- emitter (fst calResult) 1
      callback $ \adc -> do
        bus <- adc ~>* vbus
        pbI <- adc ~>* phase_b
        pcI <- adc ~>* phase_c

        tv <- deref testVoltage

        testI <- calib ~>* testCurrent
        kI    <- calib ~> calR ~>* integrator
        maxU  <- calib ~> calR ~>* maxVoltage
        secs  <- calib ~> calR ~>* seconds

        alphaI <- assign $ -0.5 * (pbI + pcI)
        newtv  <- assign $ tv + ((kI * measPeriod) * (testI - alphaI))

        safeVoltage <- call fconstrain (-maxU) maxU newtv
        voltage_modulation bus safeVoltage 0.0 pwmout

        emit timings (constRef pwmout)

        store testVoltage safeVoltage

        elapsed <- deref cyclesElapsed
        when (elapsed >=? castDefault (secs / measPeriod)) $ do
          let r = (safeVoltage / testI)
          store (calib ~> calR ~> resistance) r
          emit result $ constRef calib

        cyclesElapsed %= (+1)

  return (fst adcChan, snd calResult)
