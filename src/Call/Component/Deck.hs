{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
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
module Call.Component.Deck (emptyDeck, States, source, pos, pitch, playing, sampleRate) where
import Call.Component.Base
import Call.Component.IO
import Control.Lens
import Linear
import Call.Types
import Control.Monad.State.Strict
import Call.Data.Wave
import Control.Applicative

data States = States
  { _src :: Maybe (Source (V2 Float))
  , _pos :: Time
  , _pitch :: Double
  , _playing :: Bool
  , _sampleRate :: Double }

--
source :: Lens' States (Maybe (Source (V2 Float)))
source f s = f (_src s) <&> \a -> s { _src = a }
pos :: Lens' States Time
pos f s = f (_pos s) <&> \a -> s { _pos = a }
pitch :: Lens' States Time
pitch f s = f (_pitch s) <&> \a -> s { _pitch = a }
playing :: Lens' States Bool
playing f s = f (_playing s) <&> \a -> s { _playing = a }
sampleRate :: Lens' States Double
sampleRate f s = f (_sampleRate s) <&> \a -> s { _sampleRate = a }

emptyDeck :: Monad m => Component (AccessT States PullAudio) m
emptyDeck = stateful handle $ States Nothing 0 1 False 4410-- FIXME: sample rate

handle :: MonadState States m => PullAudio (m a) -> m a
handle (PullAudio dt0 n cont) = use source >>= \case
  Just (Source s) -> do
    pl <- use playing
    t0 <- use pos
    k <- use pitch
    let dt = dt0 * k
    if pl
      then do
        r <- use sampleRate
        pos += dt
        cont $ [s t | t <- [t0,t0 + dt / fromIntegral n..t0 + dt - 1 / r]]
      else do
        cont $ replicate n zero
  Nothing -> cont $ replicate n zero