{-# LANGUAGE ConstraintKinds, FlexibleContexts, BangPatterns #-}
module Call.Util.Text (renderer, typewriter, putStr, clear, simple) where
import Prelude hiding (putStr)
import Call.Data.Bitmap (Bitmap(..))
import Call.Data.Font
import Call.Sight
import Control.Lens hiding (simple)
import Control.Monad.Operational.Mini
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Applicative
import Control.Object
import Data.Monoid
import Linear
import Control.DeepSeq

data PushPull a b r = Push a r | Pull (b -> r)	 

(@!) :: Monad m => Object e m -> ReifiedProgram e a -> m (a, Object e m)
obj @! Return a = return (a, obj)
obj @! (e :>>= cont) = runObject obj e >>= \(a, obj') -> obj' @! cont a
infixr 3 @!

renderer :: (MonadIO m, Applicative m) => Font -> Float -> Object (Request Char (Bitmap, V2 Float, V2 Float)) m
renderer font size = flyweight (liftIO . renderChar font size)

typewriter :: MonadIO m => Float -> (Char -> m (Bitmap, V2 Float, V2 Float)) -> Object (ReifiedProgram (PushPull Char Picture)) m
typewriter l req = unfoldOM (@!) $ stateful go (V2 0 0, mempty) where
  go (Push '\3' cont) = do
    put (V2 0 0, mempty)
    return cont
  go (Push '\r' cont) = return cont
  go (Push '\n' cont) = do
    _1 . _y += l
    _1 . _x .= 0
    return cont
  go (Push ch cont) = do
    (pos, pic) <- get
    (!bmp@(Bitmap img _ _), !ofs, !adv) <- lift $ req ch
    return $! rnf img
    put (pos + adv, pic <> translate (pos + ofs) (bitmap bmp))
    return cont
  go (Pull cont) = uses _2 cont

putStr :: String -> ReifiedProgram (PushPull Char Picture) ()
putStr [] = return ()
putStr (c:cs) = Push c () :>>= const (putStr cs)

clear :: ReifiedProgram (PushPull Char Picture) ()
clear = Push '\3' () :>>= return

simple :: MonadIO m => Font -> Float -> m (String -> Picture)
simple font size = liftIO $ do
  r <- new $ renderer font size
  t <- new $ typewriter (size * 1.2) ((r.-) . request)
  return $ \s -> Picture $ applyVFX $ EmbedIO $ do
    t .- putStr s
    p <- t .- (Pull id :>>= return)
    t .- clear
    return $! unPicture p
