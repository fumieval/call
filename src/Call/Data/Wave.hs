{-# LANGUAGE BangPatterns, DeriveFunctor #-}
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
  Sample(..),
  Stereo,
  readWAVE
) where

import Data.WAVE
import Linear
import Call.Types
import Control.Monad.IO.Class
import qualified Data.Vector.Unboxed as V
import GHC.Float
import Data.Monoid
import Control.Applicative

newtype Source a = Source (Time -> a) deriving Functor

instance Applicative Source where
  pure a = Source (const a)
  Source f <*> Source g = Source (f <*> g)

instance Num a => Monoid (Source a) where
  mempty = pure 0
  mappend = liftA2 (+)

readWAVE :: MonadIO m => FilePath -> m (Sample Stereo)
readWAVE path = liftIO $ do
  WAVE h ss <- getWAVEFile path
  let vec = V.fromList (map fr ss)
      rate = fromIntegral (waveFrameRate h)
      !dur = fromIntegral (V.length vec) / rate
      sample t
        | t < 0 || t >= dur - (1/rate) = zero
        | otherwise = vec V.! round (t * rate)
  return $ Sample dur (Source sample)
  where
    fr [a, b] = V2 (double2Float $ sampleToDouble a) (double2Float $ sampleToDouble b)
    fr _ = zero

data Sample a = Sample { sampleLength :: Time ,sampleSource :: Source a}

-- TODO: Lazy processing
