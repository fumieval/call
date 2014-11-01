{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Component.Deck
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Decks that plays sounds
--
-----------------------------------------------------------------------------
module Call.Util.Deck (empty, Deck, Methods, source, pos, pitch, playing, sampleRate) where
import Control.Lens
import Linear
import Call.Types
import Control.Monad.State.Strict
import Call.Data.Wave
import Control.Object
import Data.OpenUnion1.Clean
import Call.Event

data Deck = Deck
  { _src :: Maybe (Source (V2 Float))
  , _pos :: !Time
  , _pitch :: !Float
  , _playing :: !Bool
  , _sampleRate :: !Float }

type Methods = State Deck |> Audio |> Nil
--
source :: Lens' Deck (Maybe (Source (V2 Float)))
source f s = f (_src s) <&> \a -> s { _src = a }
pos :: Lens' Deck Time
pos f s = f (_pos s) <&> \a -> s { _pos = a }
pitch :: Lens' Deck Time
pitch f s = f (_pitch s) <&> \a -> s { _pitch = a }
playing :: Lens' Deck Bool
playing f s = f (_playing s) <&> \a -> s { _playing = a }

sampleRate :: Lens' Deck Float
sampleRate f s = f (_sampleRate s) <&> \a -> s { _sampleRate = a }

empty :: Monad m => Object Methods m
empty = sharing handle $ Deck Nothing 0 1 False 44100 where -- FIXME: sample rate

handle :: MonadState Deck m => Audio a -> m a
handle = acceptM $ \(dt0, n) -> use source >>= \case
  Just (Source s) -> do
    pl <- use playing
    t0 <- use pos
    k <- use pitch
    let dt = dt0 * k
    if pl
      then do
        r <- use sampleRate
        pos += dt
        return $ map s [t0,t0 + dt / fromIntegral n..t0 + dt - 1 / r]
      else do
        return $ replicate n zero
  Nothing -> return $ replicate n zero