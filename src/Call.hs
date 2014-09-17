{-# LANGUAGE Rank2Types #-}
module Call ( -- * System
    Time,
    System,
    runSystem,
    runSystemDefault,
    MonadSystem(..),
    Control,
    -- * Component crafting
    Component(..),
    oneshot,
    HandleMouse(..),
    HandleKeyboard(..),
    Graphic(..),
    Audio(..),
    -- * Free instances
    PullGraphic(..),
    PullAudio(..),
    KeyEvent(..),
    MouseEvent(..),

    WindowMode(..),
    BoundingBox2,
    Box(..),
    isInside,
    Picture(..),
    Bitmap,
    readBitmap,
    Affine(..),
    Picture2D(..),
    Source(..),
    readWAVE,
    Key(..),
    charToKey,
    -- * IO
    liftIO,
    loadBitmapsWith,
    -- * Reexports
    module Control.Monad,
    module Control.Applicative,
    module Control.Bool,
    module Data.Color,
    module Data.Color.Names,
    module Linear,
) where

import Call.TH
import Call.Types
import Call.Component
import Call.Data.Bitmap
import Call.Data.Wave
import Call.Picture
import Call.System
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Bool
import Data.Color
import Data.Color.Names
import Linear
import Data.BoundingBox

runSystemDefault :: (forall s. System s a) -> IO (Maybe a)
runSystemDefault = runSystem Windowed (Box (V2 0 0) (V2 640 480))
