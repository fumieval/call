{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
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
-- The deck for a single stream
--
-----------------------------------------------------------------------------
module Call.Util.Deck (Deck(..), empty, source, pos, playing, playback) where
import Control.Lens
import Control.Monad.State.Class
import Call.Data.Wave
import Call.Types
import qualified Data.Vector.Storable as V

data Deck = Deck
  { _src :: Source Stereo
  , _pos :: !Time
  , _playing :: !Bool }

empty :: Deck
empty = Deck (Source $ const 0) 0 False

source :: Lens' Deck (Source Stereo)
source f s = f (_src s) <&> \a -> s { _src = a }

pos :: Lens' Deck Time
pos f s = f (_pos s) <&> \a -> s { _pos = a }

playing :: Lens' Deck Bool
playing f s = f (_playing s) <&> \a -> s { _playing = a }

playback :: MonadState Deck m => Time -> Int -> m (V.Vector Stereo)
playback dt n = do
  Source s <- use source
  pl <- use playing
  t0 <- use pos
  if pl
    then do
      pos += dt
      return $ V.fromList $ take n $ map s [t0,t0 + dt / fromIntegral n..]
    else return $ V.replicate n 0
