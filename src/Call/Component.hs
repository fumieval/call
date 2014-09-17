{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Component
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Object-oriented components
--
-----------------------------------------------------------------------------
module Call.Component where

import Call.Types
import Linear
import Call.Picture

newtype Control s (e :: * -> *) = Control Int

newtype Component e m = Component { runComponent :: forall x. e x -> m (x, Component e m) }

class HandleMouse f where
  cursorEvent :: Vec2 -> f ()
  scrollEvent :: Vec2 -> f ()
  mouseButtonEvent :: Int -> Bool -> f ()

class HandleKeyboard f where
  keyEvent :: Key -> Bool -> f ()

class Graphic e where
  pullGraphic :: Time -> e (Picture ())

class Audio e where
  pullAudio :: Time -> Int -> e [V2 Float]

oneshot :: (Functor e, Monad m) => (forall a. e (m a) -> m a) -> Component e m
oneshot m = go where
  go = Component $ \e -> m (fmap return e) >>= \a -> return (a, go)

data PullGraphic a = PullGraphic Time (Picture () -> a) deriving Functor

instance Graphic PullGraphic where
  pullGraphic t = PullGraphic t id

data PullAudio a = PullAudio Time Int ([V2 Float] -> a) deriving Functor

instance Audio PullAudio where
  pullAudio t n = PullAudio t n id
