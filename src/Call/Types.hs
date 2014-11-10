{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Types
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Call.Types (
    Time
    , Vec2
    , WindowMode(..)
    , BoundingBox2
    , MouseEvent(..)
    , Gamepad(..)
    , GamepadEvent(..)
    , Chatter(..)
    , Key(..)
    , charToKey
    , BlendMode(..)
    , Vertex(..)
    ) where

import Control.Applicative
import Linear
import Data.Typeable
import Data.BoundingBox
import Data.Char
import Foreign.Storable
import Foreign.Ptr

type Time = Float

data WindowMode = Windowed | Resizable | FullScreen deriving (Show, Eq, Ord, Read)

type Vec2 = V2 Float

type BoundingBox2 = Box V2 Float

data Chatter a = Up a | Down a

data MouseEvent = Button (Chatter Int) | Cursor Vec2 | Scroll Vec2

data Gamepad = Gamepad Int String

data GamepadEvent = PadButton Gamepad (Chatter Int) | PadConnection (Chatter Gamepad)

data Key =
      KeyUnknown
    | KeySpace
    | KeyApostrophe
    | KeyComma
    | KeyMinus
    | KeyPeriod
    | KeySlash
    | Key0
    | Key1
    | Key2
    | Key3
    | Key4
    | Key5
    | Key6
    | Key7
    | Key8
    | Key9
    | KeySemicolon
    | KeyEqual
    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF
    | KeyG
    | KeyH
    | KeyI
    | KeyJ
    | KeyK
    | KeyL
    | KeyM
    | KeyN
    | KeyO
    | KeyP
    | KeyQ
    | KeyR
    | KeyS
    | KeyT
    | KeyU
    | KeyV
    | KeyW
    | KeyX
    | KeyY
    | KeyZ
    | KeyLeftBracket
    | KeyBackslash
    | KeyRightBracket
    | KeyGraveAccent
    | KeyWorld1
    | KeyWorld2
    | KeyEscape
    | KeyEnter
    | KeyTab
    | KeyBackspace
    | KeyInsert
    | KeyDelete
    | KeyRight
    | KeyLeft
    | KeyDown
    | KeyUp
    | KeyPageUp
    | KeyPageDown
    | KeyHome
    | KeyEnd
    | KeyCapsLock
    | KeyScrollLock
    | KeyNumLock
    | KeyPrintScreen
    | KeyPause
    | KeyF1
    | KeyF2
    | KeyF3
    | KeyF4
    | KeyF5
    | KeyF6
    | KeyF7
    | KeyF8
    | KeyF9
    | KeyF10
    | KeyF11
    | KeyF12
    | KeyF13
    | KeyF14
    | KeyF15
    | KeyF16
    | KeyF17
    | KeyF18
    | KeyF19
    | KeyF20
    | KeyF21
    | KeyF22
    | KeyF23
    | KeyF24
    | KeyF25
    | KeyPad0
    | KeyPad1
    | KeyPad2
    | KeyPad3
    | KeyPad4
    | KeyPad5
    | KeyPad6
    | KeyPad7
    | KeyPad8
    | KeyPad9
    | KeyPadDecimal
    | KeyPadDivide
    | KeyPadMultiply
    | KeyPadSubtract
    | KeyPadAdd
    | KeyPadEnter
    | KeyPadEqual
    | KeyLeftShift
    | KeyLeftControl
    | KeyLeftAlt
    | KeyLeftSuper
    | KeyRightShift
    | KeyRightControl
    | KeyRightAlt
    | KeyRightSuper
    | KeyMenu
    deriving (Enum, Eq, Ord, Read, Show, Typeable, Bounded)

charToKey :: Char -> Key
charToKey ch
    | isAlpha ch = toEnum $ fromEnum KeyA + fromEnum ch - fromEnum 'A'
    | isDigit ch = toEnum $ fromEnum Key0 + fromEnum ch - fromEnum '0'
charToKey '-' = KeyMinus
charToKey ',' = KeyComma
charToKey '.' = KeyPeriod
charToKey '/' = KeySlash
charToKey ' ' = KeySpace
charToKey '\'' = KeyApostrophe
charToKey '\\' = KeyBackslash
charToKey '=' = KeyEqual
charToKey ';' = KeySemicolon
charToKey '[' = KeyLeftBracket
charToKey ']' = KeyRightBracket
charToKey '`' = KeyGraveAccent
charToKey '\n' = KeyEnter
charToKey '\r' = KeyEnter
charToKey '\t' = KeyTab
charToKey _ = KeyUnknown

data BlendMode = Normal
    | Inverse
    | Add
    | Multiply
    | Screen
    deriving (Enum, Eq, Ord, Read, Show, Typeable)

data Vertex = Vertex { vPos :: {-# UNPACK #-} !(V3 Float)
  , vUV :: {-# UNPACK #-} !(V2 Float) }

instance Storable Vertex where
  sizeOf _ = sizeOf (undefined :: V3 Float) + sizeOf (undefined :: V2 Float)
  alignment _ = 0
  peek ptr = Vertex <$> peek (castPtr ptr) <*> peek (castPtr $ ptr `plusPtr` sizeOf (vPos undefined))
  poke ptr (Vertex v t) = do
      poke (castPtr ptr) v
      poke (castPtr ptr `plusPtr` sizeOf (0 :: V3 Float)) t
