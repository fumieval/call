{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Util.Deck
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Polyphonic sampler
--
-----------------------------------------------------------------------------
module Call.Util.Sampler where
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Monad.ST
import Control.Monad.State.Strict
import Call.Types
import Call.Data.Wave
import Control.Monad.Objective
import Control.Elevator

data Sampler = Sampler [(Sample Stereo, Time)]

empty :: Sampler
empty = Sampler []

playback :: MonadState Sampler m => Time -> Int -> m (V.Vector Stereo)
playback dt n = do
  Sampler vs <- get
  let (vs'', r) = runST $ do
        v <- MV.new n
        vs' <- forM vs $ \(s0@(Sample d (Source s)), t0) -> do
          if d > t0 then return []
            else do
              forM_ [0..n-1] $ \i -> do
                z <- MV.unsafeRead v i
                MV.unsafeWrite v i $ z + s (t0 + f * fromIntegral i)
              return [(s0, t0 + dt)]
        v' <- V.unsafeFreeze v
        return (vs', v')
  put $ Sampler $ concat vs''
  return r
  where
    f = dt / fromIntegral n

play :: MonadState Sampler m => Sample Stereo -> m ()
play s = modify $ \(Sampler xs) -> Sampler $ (s, 0) : xs

playbackOf :: (MonadObjective b m, Elevate n m) => Inst b (State Sampler) n -> Time -> Int -> m (V.Vector Stereo)
playbackOf i = \dt n -> i .^ playback dt n
