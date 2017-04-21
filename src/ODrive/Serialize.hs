{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module ODrive.Serialize
  ( adcSender
  , dccalSender
  , encoderSender
  , svmSender
  , sampleSender
  , Sender
  , serializeTowerDeps
  , rateDivider
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Data.Char (ord)

import Ivory.Tower.HAL.Bus.Interface

import Ivory.Serialize
import qualified HXStream.Ivory as HX
import ODrive.Types

serializeTowerDeps :: Tower e ()
serializeTowerDeps = do
  towerDepends odriveTypes
  towerModule odriveTypes

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

-- divide rate of incoming messages by `r`
rateDivider :: (IvoryArea a, IvoryZero a)
         => Integer
         -> ChanOutput a
         -> Tower e (ChanOutput a)
rateDivider r c = do
  c' <- channel
  monitor "halfRate" $ do
    st <- stateInit "s" (ival (0 :: Uint32))
    handler c "halfRate" $ do
      e <- emitter (fst c') 1
      callback $ \v -> do
        s <- deref st
        ifte_ (s >=? fromIntegral (r - 1))
          (emit e v >> store st 0)
          (store st (s + 1))
  return (snd c')

type Sender e a = ChanOutput a -> BackpressureTransmit UARTBuffer ('Stored IBool) -> Monitor e ()

-- pack and marshal samples to backpressure transmit
sampleSender :: (ANat len, IvoryArea a, IvoryZero a, Packable a)
             => Char
             -> Proxy len
             -> Sender e a
sampleSender tag len c out = do
  buf <- stateInit "buf" $ izerolen len
  handler c "sender" $ do
    e <- emitter (backpressureTransmit out) 1
    callback $ \ sample -> do
      packInto buf 0 sample
      str <- local izero
      HX.encodeString (fromIntegral $ ord tag) (constRef buf) str
      emit e $ constRef str

adcSender :: Sender e ('Struct "adc")
adcSender = sampleSender 'A' (Proxy :: Proxy 12)

dccalSender :: Sender e ('Struct "dccal")
dccalSender = sampleSender 'D' (Proxy :: Proxy 8)

encoderSender :: Sender e ('Struct "encoder")
encoderSender = sampleSender 'E' (Proxy :: Proxy 17)

svmSender :: Sender e ('Struct "svm")
svmSender = sampleSender 'S' (Proxy :: Proxy 16)
