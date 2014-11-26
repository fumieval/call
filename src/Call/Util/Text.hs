{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Call.Util.Text where
import Control.Lens
import Data.Functor.Request
import Data.Functor.PushPull
import Control.Monad.Objective.Class
import Control.Object
import Linear
import Call.Data.Bitmap (Bitmap)
import Call.Sight
import Data.Monoid
import Control.Monad.State.Class
import Control.Elevator
import Control.Monad.Trans

type RenderChar = Request Char (Bitmap, V2 Float, V2 Float)

typewriter :: (MonadObjective m, Tower m) => Instance' RenderChar m -> Object (PushPull Char Picture) m
typewriter fon = stateful go (V2 0 0, mempty) where
  go (Push ch cont) = do
    (pos, pic) <- get
    (bmp, ofs, adv) <- lift $ fon .- request ch
    put (pos + adv, translate (pos + ofs) (bitmap bmp) <> pic)
    return cont
  go (Pull cont) = uses _2 cont
