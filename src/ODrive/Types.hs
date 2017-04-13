{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module ODrive.Types where

import Ivory.Language
import Ivory.Tower
import Ivory.Serialize

[ivory|
struct adc_sample
  { vbus    :: Stored IFloat
  ; phase_b :: Stored IFloat
  ; phase_c :: Stored IFloat
  ; meas_t  :: Stored ITime
  }

struct dccal_sample
  { dccal_b :: Stored IFloat
  ; dccal_c :: Stored IFloat
  }

struct svm_out
  { svm_a :: Stored IFloat
  ; svm_b :: Stored IFloat
  ; svm_c :: Stored IFloat
  ; svm_sextant :: Stored IFloat
  }
|]

-- from SMACCMPilot.Flight.Control.PID
-- | Constrain a floating point value to the range [xmin..xmax].
fconstrain :: Def ('[IFloat, IFloat, IFloat] ':-> IFloat)
fconstrain = proc "fconstrain" $ \xmin xmax x -> body $
  (ifte_ (x <? xmin)
    (ret xmin)
    (ifte_ (x >? xmax)
      (ret xmax)
      (ret x)))

odrive_types :: Module
odrive_types = package "odrive_types" $ do
  defStruct (Proxy :: Proxy "adc_sample")
  defStruct (Proxy :: Proxy "dccal_sample")
  defStruct (Proxy :: Proxy "svm_out")
  depend serializeModule
  wrappedPackMod svmOutWrapper
  incl fconstrain

svmOutWrapper :: WrappedPackRep ('Struct "svm_out")
svmOutWrapper = wrapPackRep "svm_out" $
  packStruct
  [ packLabel svm_a
  , packLabel svm_b
  , packLabel svm_c
  , packLabel svm_sextant
  ]

instance Packable ('Struct "svm_out") where
  packRep = wrappedPackRep svmOutWrapper
