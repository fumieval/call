{-# LANGUAGE Rank2Types #-}
module Call ( -- * System
    Time,
    MonadObjective(..),
    (.&),
    System,
    runSystem,
    runSystemDefault,
    MonadSystem(..),
    -- * Component crafting
    module Control.Object,
    Mouse(..),
    Keyboard(..),
    Graphic(..),
    Audio(..),
    -- * Free instances
    PullGraphic(..),
    PullAudio(..),
    KeyEvent(..),
    MouseEvent(..),
    -- * Concrete types
    Vec2,
    WindowMode(..),
    BoundingBox2,
    Box(..),
    isInside,
    Picture(..),
    Affine(..),
    Picture2D(..),
    BlendMode(..),
    Key(..),
    charToKey,
    -- * Bitmap
    Bitmap,
    readBitmap,
    clipBitmap,
    loadBitmapsWith,
    -- * Sound
    Source(..),
    readWAVE,
    -- * IO
    liftIO,
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
import Control.Object
import Control.Monad.Objective.Class
import Data.Color
import Data.Color.Names
import Linear
import Data.BoundingBox

runSystemDefault :: (forall s. System s a) -> IO (Maybe a)
runSystemDefault = runSystem Windowed (Box (V2 0 0) (V2 640 480))
