{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Pipes
import Pipes.Serial

import Data.Serialize
import qualified Data.ByteString.Char8 as B
import qualified HXStream.Native as HX
import qualified Pipes.Prelude as P

import LDrive.Types.Adc
import LDrive.Types.Encoder
import LDrive.Types.Svm
import LDrive.Types.CurrentControl
import LDrive.Client.Pipes
-- lp
import MVC
import qualified MVC.Prelude as MVC

import Graphics.Liveplot


msgDes :: Serialize a => B.ByteString -> IO (Maybe a)
msgDes msg = do
  case runGetState get msg 0 of
    Left e -> putStrLn ("deserialize error: " ++ e) >> return (Nothing)
    Right (r, _) -> do
      return (Just r)

des :: Proxy () (HX.Tag, B.ByteString) () (SensorReading GLfloat) IO b
des = forever $ do
  (tag, msg) <- await
  case tag of
    69 -> do
      me <- lift $ msgDes msg
      case me of
        Nothing -> return ()
        Just x -> mapM_ yield $ fromEncoder x
    65 -> do
      me <- lift $ msgDes msg
      case me of
        Nothing -> return ()
        Just x -> mapM_ yield $ fromAdc x
    83 -> do
      me <- lift $ msgDes msg
      case me of
        Nothing -> return ()
        Just x -> mapM_ yield $ fromSVM x
    67 -> do
      me <- lift $ msgDes msg
      case me of
        Nothing -> return ()
        Just x -> do
          liftIO $ print x
          mapM_ yield $ fromCC x
    _ -> return ()


des' :: (Eq a, Serialize t1, Foldable t, Foldable t2) => t2 (a, t1 -> t y) -> Proxy () (a, B.ByteString) () y IO b
des' mappings = forever $ do
  (tag, msg) <- await
  mapM_ (handle tag msg) mappings
  where
    handle tag msg (tag', handler) | tag == tag' = do
      me <- lift $ msgDes msg
      case me of
        Nothing -> return ()
        Just x -> mapM_ yield (handler x)
    handle _ _ _ = return ()

--data HXDes a = HXDes a Int

--des'' = des' [(69, fromEncoder), (65, fromAdc)]
--des'' = des' [
--  (69, toReading :: Encoder -> [SensorReading GLfloat])
--  (65, toReading :: Adc -> [SensorReading GLfloat])
--  ]

scaleRange :: Fractional a => (a, a) -> (a, a) -> a -> a
scaleRange (fromLow, fromHigh) (toLow, toHigh) x = (x - fromLow) * (toHigh - toLow) / (fromHigh - fromLow) + toLow

normRange :: Fractional a => (a, a) -> a -> a
normRange from = scaleRange from (-1, 1)

graphRange :: Fractional a => (a, a) -> a -> a
graphRange from = scaleRange from (0, 1)

-- utility pipes for liveplot
normalize :: (Fractional b, Monad m) => (b, b) -> Pipe b b m r
normalize from = P.map (graphRange from)

dbg :: (Show a) => Pipe a a IO ()
dbg = P.tee (P.map (show) >-> P.stdoutLn)

main :: IO ()
main = do
  let port = "/dev/ttyUSB0"
  s <- openSerial port defaultSerialSettings { commSpeed = CS115200 }

  runLiveplot $ myplot (hxserial s >-> des >-> dbg)

myplot :: Producer a IO ()
       -> Managed (View (Either (SensorReading GLfloat) GLApp),
                   Controller (Either a Event))
myplot pipe = do
  let sc = (9, 2)
      inits = [
         lineGraph "pll_pos" sc (0, 0)
       , lineGraph "pll_vel" sc (1, 0)
       , lineGraph "phase" sc (2, 0)
       , lineGraph "vbus" sc (3, 0)
       , lineGraph "phase_b" sc (4, 0)
       , lineGraph "phase_c" sc (5, 0)
       , lineGraph "svm_a" sc (6, 0)
       , lineGraph "svm_b" sc (7, 0)
       , lineGraph "svm_c" sc (8, 0)
       , lineGraph "alpha" sc (0, 1)
       , lineGraph "beta" sc (1, 1)
       , lineGraph "d" sc (2, 1)
       , lineGraph "q" sc (3, 1)
       , lineGraph "q_in" sc (4, 1)
       , lineGraph "q_err" sc (5, 1)
       , lineGraph "mod_d" sc (6, 1)
       , lineGraph "mod_q" sc (7, 1)
       , lineGraph "current_i_q" sc (8, 1)
       ]
  (v, c) <- ogl inits

  dat <- MVC.producer (bounded 1) (pipe)

  return (v, fmap Left (dat) <> fmap Right c)

fromEncoder :: Encoder -> [SensorReading GLfloat]
fromEncoder Encoder{..} = [
    Reading "pll_vel" $ gr' 2000000 pll_vel
  , Reading "pll_pos" $ gr' 100000 pll_pos
  , Reading "phase" $ gr' 10 phase
  ]

fromAdc :: Adc -> [SensorReading GLfloat]
fromAdc Adc{..} = [
    Reading "vbus" $ gr' 25 vbus
  , Reading "phase_b" $ gr' 0.15 (phase_b)
  , Reading "phase_c" $ gr' 0.15 (phase_c)
  ]

fromSVM :: Svm -> [SensorReading GLfloat]
fromSVM Svm{..} = [
    Reading "svm_a" $ gr' 1 svm_a
  , Reading "svm_b" $ gr' 1 (svm_b)
  , Reading "svm_c" $ gr' 1 (svm_c)
  ]

fromCC :: CurrentControl -> [SensorReading GLfloat]
fromCC CurrentControl{..} = [
    Reading "alpha" $ gr' 1 alpha
  , Reading "beta" $ gr' 1 beta
  , Reading "d" $ gr' 1 d
  , Reading "q" $ gr' 1 q
  , Reading "d_in" $ gr' 1 d_in
  , Reading "q_in" $ gr' 1 q_in
  , Reading "d_err" $ gr' 1 d_err
  , Reading "q_err" $ gr' 1 q_err
  , Reading "v_d" $ gr' 1 v_d
  , Reading "v_q" $ gr' 1 v_q
  , Reading "mod_d" $ gr' 1 mod_d
  , Reading "mod_q" $ gr' 1 mod_q
  , Reading "current_i_q" $ gr' 10 current_i_q
  ]

gr' :: GLfloat -> Float -> GLfloat
gr' x = (graphRange (-x, x) :: Float -> GLfloat)

gR :: (GLfloat, GLfloat) -> Float -> GLfloat
gR (x1, x2) = (graphRange (x1, x2) :: Float -> GLfloat)

class ToReading a where
  toReading :: a -> [SensorReading GLfloat]

instance ToReading Encoder where
  toReading = fromEncoder

instance ToReading Adc where
  toReading = fromAdc
