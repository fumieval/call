{-# LANGUAGE FlexibleContexts, GADTs #-}
module Call.Util where
import Control.Object
import Call.Types
import Call.Sight
import Control.Monad.State
import qualified Call.Data.Bitmap as Bitmap
import Data.Functor.Request

readBitmap :: MonadIO m => FilePath -> m Bitmap.Bitmap
readBitmap = Bitmap.readFile
