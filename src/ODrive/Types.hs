{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

struct svm_sample
  { svm_a :: Stored IFloat
  ; svm_b :: Stored IFloat
  ; svm_c :: Stored IFloat
  ; svm_sextant :: Stored IFloat
  }
|]

[ivory| string struct UARTBuffer 128 |]

uartTestTypes :: Module
uartTestTypes = package "uartTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)


-- from SMACCMPilot.Flight.Control.PID
-- | Constrain a floating point value to the range [xmin..xmax].
fconstrain :: Def ('[IFloat, IFloat, IFloat] ':-> IFloat)
fconstrain = proc "fconstrain" $ \xmin xmax x -> body $
  (ifte_ (x <? xmin)
    (ret xmin)
    (ifte_ (x >? xmax)
      (ret xmax)
      (ret x)))

odriveTypes :: Module
odriveTypes = package "odrive_types" $ do
  incl fconstrain

  defStruct (Proxy :: Proxy "adc_sample")
  defStruct (Proxy :: Proxy "dccal_sample")
  defStruct (Proxy :: Proxy "svm_sample")

  defStringType (Proxy :: Proxy UARTBuffer)

  depend serializeModule

  wrappedPackMod svmSampleWrapper
  wrappedPackMod adcSampleWrapper
  wrappedPackMod dccalSampleWrapper


adcSampleWrapper :: WrappedPackRep ('Struct "adc_sample")
adcSampleWrapper = wrapPackRep "adc_sample" $
  packStruct
  [ packLabel vbus
  , packLabel phase_b
  , packLabel phase_c
  ]

instance Packable ('Struct "adc_sample") where
  packRep = wrappedPackRep adcSampleWrapper

dccalSampleWrapper :: WrappedPackRep ('Struct "dccal_sample")
dccalSampleWrapper = wrapPackRep "dccal_sample" $
  packStruct
  [ packLabel dccal_b
  , packLabel dccal_c
  ]

instance Packable ('Struct "dccal_sample") where
  packRep = wrappedPackRep dccalSampleWrapper

svmSampleWrapper :: WrappedPackRep ('Struct "svm_sample")
svmSampleWrapper = wrapPackRep "svm_sample" $
  packStruct
  [ packLabel svm_a
  , packLabel svm_b
  , packLabel svm_c
  , packLabel svm_sextant
  ]

instance Packable ('Struct "svm_sample") where
  packRep = wrappedPackRep svmSampleWrapper
