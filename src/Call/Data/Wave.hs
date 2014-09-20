{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Data.Wave
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Call.Data.Wave (
  Source(..),
  Stereo,
  readWAVE
) where

import Data.WAVE
import Linear
import Call.Types
import Control.Monad.IO.Class
import qualified Data.Vector.Unboxed as V

newtype Source a = Source (Time -> a)

type Stereo = V2 Float

readWAVE :: MonadIO m => FilePath -> m (Source Stereo)
readWAVE path = liftIO $ do
  WAVE h ss <- getWAVEFile path
  
  return $ Source $ sample h $ V.fromList (map fr ss)
  where
    fr [!a, !b] = (realToFrac $ sampleToDouble a, realToFrac $ sampleToDouble b)
    fr _ = (0, 0)
    sample h v t = maybe zero (uncurry V2) $ v V.!? floor (t * fromIntegral (waveFrameRate h))

-- TODO: Lazy processing