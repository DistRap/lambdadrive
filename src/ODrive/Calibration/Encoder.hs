{-# LANGUAGE DataKinds #-}
module ODrive.Calibration.Encoder where

import Ivory.Language
import Ivory.Language.Cast
import Ivory.Stdlib
import Ivory.Tower

import ODrive.Types
import ODrive.Control.Modulation
import ODrive.Ivory.Types.Adc
import ODrive.Ivory.Types.Encoder
import ODrive.Ivory.Types.AdcEncSample
import ODrive.Ivory.Types.CalEnc
import ODrive.Ivory.Types.CalError
import ODrive.Ivory.Types.Calibration

encoderOffsetTower :: IFloat
                   -> ChanOutput ('Struct "calibration")
                   -> PWMInput
                   -> Tower e (
                         ChanInput ('Struct "adc_enc_sample")
                       , ChanOutput ('Struct "calibration"))
encoderOffsetTower measHz calOut timingsIn = do
  adcEncChan <- channel
  calResult <- channel

  monitor "cal_enc_offset" $ do
    pwmout <- stateInit "pwmout" $ iarray [ival 0, ival 0, ival 0]
    stepSize <- stateInit "stepSize" $ ival (0.0 :: IFloat)
    scanFwd <- stateInit "scanFwd" $ ival true
    currentPhase <- stateInit "currentPhase" $ ival (0.0 :: IFloat)
    phaseBound <- stateInit "phaseBound" $ ival (0.0 :: IFloat)
    encOffSum <- stateInit "encOffSum" $ ival (0 :: Sint32)
    encOffDtStep <- stateInit "encOffDtStep" $ ival $ (1/500.0 :: IFloat)
    encOffDtCurr <- stateInit "encOffDtCurr" $ ival $ 0.0
    initCnt <- state "initCnt"
    firstEncSample <- state "fstEncSample"

    calib <- state "calib"
    sumFwd <- state "sumFwd"
    sumBack <- state "sumBack"
    msgCount <- stateInit "msgCount" $ ival (0 :: Uint32)

    handler calOut "cal_enc_offset" $ do
      callback $ \x -> do
        refCopy calib x

        range <- calib ~> calEnc ~>* scanRange
        nsteps <- calib ~> calEnc ~>* steps

        store stepSize (range / safeCast nsteps)

        store currentPhase ((-range) / 2)
        store phaseBound (range / 2)

    handler (snd adcEncChan) "adc_enc_offset" $ do
      timings <- emitter timingsIn 1
      result <- emitter (fst calResult) 1
      callback $ \ae -> do
        msgCount %= (+1)

        fstEncSample <- deref firstEncSample
        when fstEncSample $ do
          store firstEncSample false

          cnt <- ae ~> enc_sample ~>* count
          store initCnt cnt

        step <- deref stepSize

        dts <- deref encOffDtStep
        dtc <- deref encOffDtCurr

        vm <- calib ~>* voltageMag

        ph <- deref currentPhase
        bound <- deref phaseBound

        fwd <- deref scanFwd

        -- stay on this step for dts * measHz time
        ifte_ (dtc >=? dts * measHz)
          (do
              ifte_ fwd
                (store currentPhase (ph + step))
                (store currentPhase (ph - step))

              store encOffDtCurr 0.0

              cs <- deref encOffSum
              cnt <- ae ~> enc_sample ~>* count
              store encOffSum (cs + cnt)
              )
          (store encOffDtCurr (dtc + 1))

        bus <- ae ~> adc_sample ~>* vbus
        ph' <- deref currentPhase
        let vA = vm * cos ph'
            vB = vm * sin ph'
        voltage_modulation bus vA vB pwmout
        emit timings (constRef pwmout)

        when (fwd .&& ph >=? bound) $ do
          store scanFwd false
          store currentPhase (bound)

          cs <- deref encOffSum
          store sumFwd cs

          cnt <- ae ~> enc_sample ~>* count
          iCnt <- deref initCnt
          cond_ [
              cnt >? iCnt + 8 ==> store (calib ~> calEnc ~> direction)   1
            , cnt <? iCnt - 8 ==> store (calib ~> calEnc ~> direction) (-1)
            , true ==> store (calib ~> calErr) noEncoderResponse
            ]

        when (iNot fwd .&& ph <=? -bound) $ do
          cs <- deref encOffSum
          store sumBack cs
          nsteps <- calib ~> calEnc ~>* steps
          off <- assign $ ((ivoryCast :: Sint32 -> IFloat) cs) / (safeCast nsteps * 2)
          store (calib ~> calEnc ~> offset) off
          emit result $ constRef calib

  return (fst adcEncChan, snd calResult)
