{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Call.Util.Text where
import Control.Lens
import Data.Functor.Request
import Data.Functor.PushPull
import Control.Object
import Linear
import Call.Data.Bitmap (Bitmap)
import Call.Sight
import Data.Monoid
import Control.Monad.State.Class
import Control.Monad.Trans
import Call.Data.Font

renderer :: MonadIO m => Font -> Float -> Object (Request Char (Bitmap, V2 Float, V2 Float)) m
renderer font size = flyweight (liftIO . renderChar font size)

typewriter :: MonadIO m => (Char -> m (Bitmap, V2 Float, V2 Float)) -> Object (PushPull Char Picture) m
typewriter req = stateful go (V2 0 0, mempty) where
  go (Push ch cont) = do
    (pos, pic) <- get
    (bmp, ofs, adv) <- lift $ req ch
    put (pos + adv, translate (pos + ofs) (bitmap bmp) <> pic)
    return cont
  go (Pull cont) = uses _2 cont