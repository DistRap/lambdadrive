{-# LANGUAGE DataKinds #-}
module ODrive.Calibration.Inductance where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import ODrive.Types
import ODrive.Control.Modulation
import ODrive.Ivory.Types.CalI
import ODrive.Ivory.Types.Calibration
import ODrive.Ivory.Types.Adc

phaseInductanceTower :: IFloat
                     -> ChanOutput ('Struct "calibration")
                     -> PWMInput
                     -> Tower e (
                            ChanInput ('Struct "adc")
                          , ChanOutput ('Struct "calibration"))
phaseInductanceTower measPeriod calOut timingsIn = do
  adcChan <- channel
  calResult <- channel

  monitor "cal_phase_inductance" $ do
    pwmout <- stateInit "pwmout" $ iarray [ival 0, ival 0, ival 0]
    cyclesElapsed <- stateInit "cyclesElapsed" $ ival (0 :: Uint16)
    stateLow <- stateInit "inductanceLow" $ ival true

    calib <- state "calib"

    handler calOut "cal_phase_resistance" $ do
      callback $ refCopy calib

    handler (snd adcChan) "adc_phase_inductance" $ do
      timings <- emitter timingsIn 1
      result <- emitter (fst calResult) 1
      callback $ \adc -> do
        bus <- adc ~>* vbus
        pbI <- adc ~>* phase_b
        pcI <- adc ~>* phase_c

        tvl <- calib ~> calI ~>* testVoltageLow
        tvh <- calib ~> calI ~>* testVoltageHigh

        isStateLow <- deref stateLow
        ifte_ isStateLow
          (do
              store stateLow false

              ca <- calib ~> calI ~>* alphasLow
              store (calib ~> calI ~> alphasLow) (ca + (-pbI - pcI))

              voltage_modulation bus tvl 0.0 pwmout
              emit timings (constRef pwmout)
              )
          (do
              store stateLow true

              ca <- calib ~> calI ~>* alphasHigh
              store (calib ~> calI ~> alphasHigh) (ca + (-pbI - pcI))

              voltage_modulation bus tvh 0.0 pwmout
              emit timings (constRef pwmout)
              )

        maxc <- calib ~> calI ~>* cycles
        elapsed <- deref cyclesElapsed
        when (elapsed >=? maxc * 2) $ do
          al <- calib ~> calI ~>* alphasLow
          ah <- calib ~> calI ~>* alphasHigh

          let vL = ((tvh - tvl) / 2)
              dIbydT = (ah - al) / (measPeriod * (safeCast maxc))
              res = vL / dIbydT

          store (calib ~> calI ~> inductance) res
          emit result $ constRef calib

        cyclesElapsed %= (+1)

  return (fst adcChan, snd calResult)
