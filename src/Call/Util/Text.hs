{-# LANGUAGE ConstraintKinds, FlexibleContexts, BangPatterns, DeriveFunctor #-}
module Call.Util.Text (renderer, typewriter, putStr, clear, simple) where
import Prelude hiding (putStr)
import Call.Data.Bitmap (Bitmap(..))
import Call.Data.Font
import Call.Sight
import Control.Lens hiding (simple)
import Control.Monad.Free
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Applicative
import Control.Object
import Data.Monoid
import Linear
import Control.DeepSeq

data Typewriting a = Render (Picture -> a) | Type Char a deriving Functor

renderer :: (MonadIO m, Applicative m) => Font -> Float -> Object (Request Char (Bitmap, V2 Float, V2 Float)) m
renderer font size = flyweight (liftIO . renderChar font size)

typewriter :: MonadIO m => Float -> (Char -> m (Bitmap, V2 Float, V2 Float)) -> Object (Free Typewriting) m
typewriter l req = iterative $ stateful go (V2 0 0, mempty) where
  go (Type '\3' cont) = do
    put (V2 0 0, mempty)
    return cont
  go (Type '\r' cont) = return cont
  go (Type '\n' cont) = do
    _1 . _y += l
    _1 . _x .= 0
    return cont
  go (Type ch cont) = do
    (pos, pic) <- get
    (!bmp@(Bitmap img _ _), !ofs, !adv) <- lift $ req ch
    return $! rnf img
    put (pos + adv, pic <> translate (pos + ofs) (bitmap bmp))
    return cont
  go (Render cont) = uses _2 cont

putStr :: String -> Free Typewriting ()
putStr [] = return ()
putStr (c:cs) = wrap (Type c (putStr cs))

clear :: Free Typewriting ()
clear = liftF $ Type '\3' ()

simple :: MonadIO m => Font -> Float -> m (String -> Picture)
simple font size = liftIO $ do
  r <- new $ renderer font size
  t <- new $ typewriter (size * 1.2) ((r.-) . request)
  return $ \s -> Picture $ applyVFX $ EmbedIO $ do
    t .- putStr s
    p <- t .- liftF (Render id)
    t .- clear
    return $! unPicture p
