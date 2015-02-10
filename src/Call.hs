{-# LANGUAGE Rank2Types #-}
module Call ( -- * System
    runSystemDefault,
    readBitmap,
    module Call.System,
    -- * Reexports
    module Data.Graphics,
    module Data.Audio,
    module Control.Monad,
    module Control.Applicative,
    module Control.Bool,
    module Data.Monoid,
    module Data.Color,
    module Data.Color.Names,
    module Linear,
    module Control.Object,
    module Control.Monad.Objective.Class,
    module Control.Monad.IO.Class
) where

import Data.Graphics
import qualified Data.Graphics.Bitmap as Bitmap
import Data.Audio
import Call.System
import Call.Internal.GLFW hiding (System)
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Bool
import Control.Object
import Control.Monad.Objective.Class
import Data.Color
import Data.Color.Names
import Data.Monoid
import Linear
import Data.BoundingBox

runSystemDefault :: (forall s. System s a) -> IO (Maybe a)
runSystemDefault = runSystem Windowed (Box (V2 0 0) (V2 640 480))

readBitmap :: MonadIO m => FilePath -> m Bitmap.Bitmap
readBitmap = Bitmap.readFile
