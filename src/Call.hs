{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module Call ( -- * System
    runCallDefault,
    readBitmap,
    module Call.Sight,
    module Call.System,
    module Call.Types,
    module Call.TH,
    module Call.Data.Wave,
    module Call.Data.Font,
    -- * Reexports
    module Control.Monad,
    module Control.Applicative,
    module Control.Bool,
    module Data.Monoid,
    module Data.Color,
    module Data.Color.Names,
    module Linear,
    module Control.Object,
    module Control.Monad.IO.Class
) where

import Call.TH
import Call.Types
import Call.Data.Wave
import Call.Data.Font
import Call.Sight
import Call.System
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Bool
import Control.Object
import Data.Color
import Data.Color.Names
import Data.Monoid
import Linear
import Data.BoundingBox
import qualified Call.Data.Bitmap as Bitmap

runCallDefault :: (Call => IO a) -> IO (Maybe a)
runCallDefault = runCall Windowed (Box (V2 0 0) (V2 640 480))

readBitmap :: MonadIO m => FilePath -> m Bitmap.Bitmap
readBitmap = Bitmap.readFile
