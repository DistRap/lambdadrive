{-# LANGUAGE DataKinds #-}

module ODrive.Calibration where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Ivory.BSP.STM32.ClockConfig

import ODrive.Platforms (currentMeasPeriod, currentMeasHz)
import ODrive.Types
import ODrive.Calibration.Inductance
import ODrive.Calibration.Encoder
import ODrive.Calibration.Resistance
import ODrive.Calibration.Lock
import LDrive.Ivory.Types.Calibration
import LDrive.Ivory.Types.CalI
import LDrive.Ivory.Types.CalEnc
import LDrive.Ivory.Types.CalR
import LDrive.Ivory.Types.AdcEncSample

calibrationTower :: ClockConfig
                 -> PWMInput
                 -> Tower e (
                       ChanInput ('Struct "adc_enc_sample")
                     , ChanOutput ('Struct "calibration"))
calibrationTower cc timingsIn = do
  adcEncChan <- channel
  calResult <- channel
  (calIn, calOut) <- channel

  let measPeriod = currentMeasPeriod cc
  let measHz = currentMeasHz cc

  (phaseRIn, phaseRDone) <- phaseResistanceTower measPeriod calOut timingsIn
  (phaseIIn, phaseIDone) <- phaseInductanceTower measPeriod calOut timingsIn
  (lockIn, lockDone) <- phaseLockTower measHz calOut timingsIn
  (encOffIn, encOffDone) <- encoderOffsetTower measHz calOut timingsIn

  monitor "calibration_controller" $ do

    calib <- state "calibState"
    calibPhaseResistance <- stateInit "calibPhaseResistance" $ ival false
    calibPhaseInductance <- stateInit "calibPhaseInductance" $ ival false
    calibPhaseLock <- stateInit "calibPhaseLock" $ ival false
    calibEncoder <- stateInit "calibEncoder" $ ival false
    adcEnc <- state "adcEnc"

    handler systemInit "init" $ do
      calUpdate <- emitter calIn 1
      callback $ const $ do

        store (calib ~> testCurrent) 10.0
        store (calib ~> calR ~> maxVoltage) 1.0
        store (calib ~> calR ~> integrator) 10.0
        store (calib ~> calR ~> seconds) 3.0

        store (calib ~> calI ~> testVoltageLow) (-1.0)
        store (calib ~> calI ~> testVoltageHigh)  1.0
        store (calib ~> calI ~> cycles) 5000

        store (calib ~> calEnc ~> steps) 1024
        store (calib ~> calEnc ~> scanRange) (4 * pi)

        emit calUpdate $ constRef calib
        store calibPhaseResistance true

    handler phaseRDone "phaseRDone" $ do
      calUpdate <- emitter calIn 1
      callback $ \x -> do
        refCopy calib x
        store calibPhaseResistance false
        store calibPhaseInductance true

        tc <- calib ~>* testCurrent
        r <- calib ~> calR ~>* resistance
        store (calib ~> voltageMag) $ tc * r

        emit calUpdate $ constRef calib

    handler phaseIDone "phaseIDone" $ do
      calUpdate <- emitter calIn 1
      callback $ \x -> do
        refCopy calib x
        emit calUpdate $ constRef calib

        store calibPhaseInductance false
        store calibPhaseLock true

    handler lockDone "phaseLockDone" $ do
      calUpdate <- emitter calIn 1
      callback $ \x -> do
        refCopy calib x
        emit calUpdate $ constRef calib

        store calibPhaseLock false
        store calibEncoder true

    handler encOffDone "encOffDone" $ do
      calUpdate <- emitter calIn 1
      calDone <- emitter (fst calResult) 1
      callback $ \x -> do
        refCopy calib x
        emit calUpdate $ constRef calib

        store calibEncoder false

        emit calDone $ constRef calib

    handler (snd adcEncChan) "adc_chan" $ do
      phaseR <- emitter phaseRIn 1
      phaseI <- emitter phaseIIn 1
      lock <- emitter lockIn 1
      encOff <- emitter encOffIn 1
      callback $ \ae -> do
        refCopy adcEnc ae

        isPhaseR <- deref calibPhaseResistance
        when isPhaseR $ do
          emit phaseR $ constRef (adcEnc ~> adc_sample)

        isPhaseI <- deref calibPhaseInductance
        when isPhaseI $ do
          emit phaseI $ constRef (adcEnc ~> adc_sample)

        isPhaseLock <- deref calibPhaseLock
        when isPhaseLock $ do
          emit lock $ constRef (adcEnc ~> adc_sample)

        isCalibEncoder <- deref calibEncoder
        when isCalibEncoder $ do
          emit encOff $ ae

  return (fst adcEncChan, snd calResult)
