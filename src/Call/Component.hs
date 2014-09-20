{-# LANGUAGE DeriveFunctor #-}
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
-----------------------------------------------------------------------------
module Call.Component where

import Call.Types
import Linear
import Call.Picture
import Control.Object

class Mouse f where
  cursorEvent :: Vec2 -> f ()
  scrollEvent :: Vec2 -> f ()
  mouseButtonEvent :: Int -> Bool -> f ()

class Keyboard f where
  keyEvent :: Key -> Bool -> f ()

class Graphic e where
  pullGraphic :: Time -> e (Picture ())

class Audio e where
  pullAudio :: Time -> Int -> e [V2 Float]

data PullGraphic a = PullGraphic !Time (Picture () -> a) deriving Functor

instance Graphic PullGraphic where
  pullGraphic t = PullGraphic t id

data PullAudio a = PullAudio !Time !Int ([V2 Float] -> a) deriving Functor

instance Audio PullAudio where
  pullAudio t n = PullAudio t n id

data KeyEvent a = KeyEvent !Key !Bool a deriving Functor

instance Keyboard KeyEvent where
  keyEvent k b = KeyEvent k b ()

data MouseEvent a = CursorEvent !Vec2 a | ScrollEvent !Vec2 a | MouseButtonEvent !Int !Bool a

instance Mouse MouseEvent where
  cursorEvent v = CursorEvent v ()
  scrollEvent v = ScrollEvent v ()
  mouseButtonEvent i b = MouseButtonEvent i b ()

------------------------------------------------------------------

instance Mouse e => Mouse (AccessT s e) where
  cursorEvent v = LiftAccessT (cursorEvent v)
  scrollEvent v = LiftAccessT (scrollEvent v)
  mouseButtonEvent i b = LiftAccessT (mouseButtonEvent i b)

instance Keyboard e => Keyboard (AccessT s e) where
  keyEvent k b = LiftAccessT (keyEvent k b)

instance Graphic e => Graphic (AccessT s e) where
  pullGraphic dt = LiftAccessT (pullGraphic dt)

instance Audio e => Audio (AccessT s e) where
  pullAudio dt n = LiftAccessT (pullAudio dt n)
