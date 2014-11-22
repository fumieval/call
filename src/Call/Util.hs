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

animate :: Monad m => (Time -> Sight) -> Object (Request Time Sight) m
animate f = go (0 :: Float) where
  go t = Object $ \(Request dt cont) -> return (cont $ f t, go (t + dt))

transit :: MonadPlus m => Time -> (Time -> Sight) -> Object (Request Time Sight) m
transit len f = go 0 where
  go t
    | t >= len = Object $ const mzero
    | otherwise = Object $ \(Request dt cont) -> return (cont $ f (t / len), go (t + dt))
