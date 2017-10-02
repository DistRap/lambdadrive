module LDrive.Client.Pipes where

import Pipes
import Pipes.Serial
import Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Internal (c2w)
import           Data.Word
import Data.Serialize

import qualified HXStream.Native as HX

--- XXX: move some of these to ivory-tower-hxstream

hxserial :: MonadIO m => SerialPort -> Proxy a' a () (HX.Tag, B.ByteString) m ()
hxserial s = fromSerial s >-> hxDecoder

hxbytag :: (Serialize a) => Word8 -> Pipe (HX.Tag, B.ByteString) a IO ()
hxbytag tag = hxUntagger tag >-> msgDeserialize

bytestringUnpack :: Monad m => Pipe B.ByteString Word8 m ()
bytestringUnpack = forever $ do
  bs <- await
  mapM_ (yield . c2w) (B.unpack bs)

hxDecoder :: Monad m => Pipe B.ByteString (HX.Tag, B.ByteString) m ()
hxDecoder = bytestringUnpack >-> aux HX.emptyStreamState
  where
  aux ss = do
     b <- await
     let (mf, ss') = HX.decodeByte b ss
     case mf of
       Just tbs -> yield tbs
       Nothing -> return ()
     aux ss'

hxUntagger :: Monad m => HX.Tag -> Pipe (HX.Tag, a) a m ()
hxUntagger t = forever $ do
  (t', a) <- await
  when (t == t') $ yield a

msgDeserialize :: (Serialize a) => Pipe B.ByteString a IO ()
msgDeserialize = forever $ do
  ser <- await
  process ser
  where
  process ser = case runGetState get ser 0 of
    Left e -> lift $ putStrLn ("deserialize error: " ++ e)
    Right (r, rest) -> do
      yield r
      unless (B.null rest || B.all (== (toEnum 0)) rest) $ process rest

msgSerialize :: (Serialize a, Monad m) => Pipe a B.ByteString m ()
msgSerialize = forever $ do
  msg <- await
  yield (runPut (put msg))
