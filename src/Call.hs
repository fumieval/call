{-# LANGUAGE Rank2Types #-}
module Call ( -- * System
    runSystemDefault,
    module Call.Sight,
    module Call.Util,
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
    module Control.Monad.Objective.Class,
    module Control.Monad.IO.Class
) where

import Call.TH
import Call.Types
import Call.Data.Wave
import Call.Data.Font
import Call.Sight
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
import Data.Monoid
import Linear
import Data.BoundingBox

runSystemDefault :: (forall s. System s a) -> IO (Maybe a)
runSystemDefault = runSystem Windowed (Box (V2 0 0) (V2 640 480))
