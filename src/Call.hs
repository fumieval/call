{-# LANGUAGE Rank2Types #-}
module Call ( -- * System
    Time,
    MonadObjective(..),
    (.&),
    (.<<),
    (.^>),
    System,
    runSystem,
    runSystemDefault,
    MonadSystem(..),
    ObjS,
    AddrS,
    -- * Events
    Chatter(..),
    MouseEvent(..),
    Graphic,
    Audio,
    Keyboard,
    Mouse,
    -- * Concrete types
    Vec2,
    WindowMode(..),
    BoundingBox2,
    Box(..),
    isInside,
    Picture(..),
    Affine(..),
    Picture2D(..),
    opacity,
    BlendMode(..),
    Key(..),
    charToKey,
    -- * Bitmap
    Bitmap,
    readBitmap,
    loadBitmapsWith,
    -- * Sound
    Source(..),
    readWAVE,
    -- * IO
    liftIO,
    -- * Utilities
    animate,
    transit,
    withSound,
    -- * Reexports
    module Control.Monad,
    module Control.Applicative,
    module Control.Bool,
    module Data.Color,
    module Data.Color.Names,
    module Linear,
    module Control.Object
) where

import Call.TH
import Call.Types
import Call.Event
import Call.Data.Bitmap
import Call.Data.Wave
import Call.Picture
import Call.System
import Call.Util
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
