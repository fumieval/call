{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Call.Internal.PortAudio(Error(..), with) where

import Bindings.PortAudio
import Foreign.C.Types
import Foreign hiding (with)
import Control.Monad.IO.Class
import Control.Monad
import Linear
import Control.Exception
import Data.Typeable
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

data Error = NotInitialized
  | UnanticipatedHostError
  | InvalidChannelCount
  | InvalidSampleRate
  | InvalidDevice
  | InvalidFlag
  | SampleFormatNotSupported
  | BadIODeviceCombination
  | InsufficientMemory
  | BufferTooBig
  | BufferTooSmall
  | NullCallback
  | BadStreamPtr
  | TimedOut
  | InternalError
  | DeviceUnavailable
  | IncompatibleHostApiSpecificStreamInfo
  | StreamIsStopped
  | StreamIsNotStopped
  | InputOverflowed
  | OutputUnderflowed
  | HostApiNotFound
  | InvalidHostApi
  | CanNotReadFromACallbackStream
  | CanNotWriteToACallbackStream
  | CanNotReadFromAnOutputOnlyStream
  | CanNotWriteToAnInputOnlyStream
  | IncompatibleStreamHostApi
  | BadBufferPtr
  deriving (Show, Eq, Ord, Enum, Typeable)

instance Exception Error

fromErrorCode :: CInt -> Error
fromErrorCode n = toEnum (fromIntegral n + 10000)
{-
data Host = Host Int String

getHostApis :: MonadIO m => m [Host]
getHostApis = liftIO $ do
    n <- c'Pa_GetHostApiCount
    forM [0..n - 1] $ \i -> do
      info <- c'Pa_GetHostApiInfo i >>= peek
      name <- peekCAString $ c'PaHostApiInfo'name info
      return (Host (fromIntegral i) name)
-}
callback :: (MV.IOVector (V2 Float) -> IO ()) -> Ptr () -> Ptr () -> CULong -> x -> y -> z -> IO CUInt
callback f _ (castPtr -> pout) (fromIntegral -> n) _ _ _ = do
  fp <- newForeignPtr_ pout
  f $ MV.unsafeFromForeignPtr0 fp n
  return c'paContinue

with :: MonadIO m => Float -> Int -> (MV.IOVector (V2 Float) -> IO ()) -> m a -> m a
with rate buf f m = do
  w c'Pa_Initialize
  cb <- liftIO $ mk'PaStreamCallback $ callback f

  ps <- liftIO malloc
  w $ c'Pa_OpenDefaultStream ps
      0
      2
      1 -- Float
      (realToFrac rate)
      (fromIntegral buf)
      cb
      nullPtr
  s <- liftIO $ peek ps

  w $ c'Pa_StartStream s
  r <- m
  w $ c'Pa_StopStream s
  w $ c'Pa_CloseStream s
  w $ c'Pa_Terminate
  return r
  where
    w n = do
      r <- liftIO n
      unless (r == 0) $ liftIO $ throwIO $ fromErrorCode r
