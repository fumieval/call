{-# LANGUAGE DeriveFunctor #-}
module Call.Component.IO where

import Call.Types
import Linear
import Call.Picture
import Call.Component.Base

class HandleMouse f where
  cursorEvent :: Vec2 -> f ()
  scrollEvent :: Vec2 -> f ()
  mouseButtonEvent :: Int -> Bool -> f ()

instance HandleMouse e => HandleMouse (AccessT s e) where
  cursorEvent v = LiftAccessT (cursorEvent v)
  scrollEvent v = LiftAccessT (scrollEvent v)
  mouseButtonEvent i b = LiftAccessT (mouseButtonEvent i b)

class HandleKeyboard f where
  keyEvent :: Key -> Bool -> f ()

instance HandleKeyboard e => HandleKeyboard (AccessT s e) where
  keyEvent k b = LiftAccessT (keyEvent k b)

class Graphic e where
  pullGraphic :: Time -> e (Picture ())

instance Graphic e => Graphic (AccessT s e) where
  pullGraphic dt = LiftAccessT (pullGraphic dt)

class Audio e where
  pullAudio :: Time -> Int -> e [V2 Float]

instance Audio e => Audio (AccessT s e) where
  pullAudio dt n = LiftAccessT (pullAudio dt n)

data PullGraphic a = PullGraphic Time (Picture () -> a) deriving Functor

instance Graphic PullGraphic where
  pullGraphic t = PullGraphic t id

data PullAudio a = PullAudio Time Int ([V2 Float] -> a) deriving Functor

instance Audio PullAudio where
  pullAudio t n = PullAudio t n id

data KeyEvent a = KeyEvent Key Bool a deriving Functor

instance HandleKeyboard KeyEvent where
  keyEvent k b = KeyEvent k b ()

data MouseEvent a = CursorEvent Vec2 a | ScrollEvent Vec2 a | MouseButtonEvent Int Bool a

instance HandleMouse MouseEvent where
  cursorEvent v = CursorEvent v ()
  scrollEvent v = ScrollEvent v ()
  mouseButtonEvent i b = MouseButtonEvent i b ()