{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module LDrive.Control.Modulation where

import Ivory.Language
import Ivory.Stdlib

import LDrive.Platforms (tim_period_clocks)
import LDrive.Control.SVM (svm)
import LDrive.Ivory.Types.Svm

current_modulation :: (GetAlloc eff ~ 'Scope s)
              => IFloat
              -> IFloat
              -> Ref s1 ('Array 3 ('Stored Uint16))
              -> Ivory eff ()
current_modulation alpha beta pwmout = do
        svmout <- svm alpha beta
        pA <- deref (svmout ~> svm_a)
        pB <- deref (svmout ~> svm_b)
        pC <- deref (svmout ~> svm_c)
        store (pwmout ! 0) (castDefault $ pA * tim_period_clocks_float)
        store (pwmout ! 1) (castDefault $ pB * tim_period_clocks_float)
        store (pwmout ! 2) (castDefault $ pC * tim_period_clocks_float)
  where
    tim_period_clocks_float :: IFloat
    tim_period_clocks_float = safeCast tim_period_clocks

voltage_modulation :: (GetAlloc eff ~ 'Scope s)
              => IFloat
              -> IFloat
              -> IFloat
              -> Ref s1 ('Array 3 ('Stored Uint16))
              -> Ivory eff ()
voltage_modulation vbus alpha beta pwmout = do
    let vfactor = 1.0 / ((2.0 / 3.0) * vbus)
    current_modulation (vfactor * alpha) (vfactor * beta) pwmout
