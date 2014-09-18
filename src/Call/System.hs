{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.System
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Call.System (System, runSystem, MonadSystem(..)) where

import Call.Component
import Call.Data.Bitmap
import Call.Picture
import Call.Types
import Control.Applicative
import Control.Artery
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Object
import Control.Monad.Objective.Class
import Data.IORef
import Data.Reflection
import GHC.Prim
import Linear
import Linear.V
import qualified Call.Internal.GLFW as G
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import qualified System.PortAudio as PA
import Unsafe.Coerce

class (MonadIO m, MonadObjective s m) => MonadSystem s m where
  linkMouse :: HandleMouse e => Control s e -> m ()
  linkKeyboard :: HandleKeyboard e => Control s e -> m ()
  linkGraphic :: Graphic e => Control s e -> m ()
  linkAudio :: Audio e => Control s e -> m ()
  unlinkMouse :: Control s e -> m ()
  unlinkKeyboard :: Control s e -> m ()
  unlinkGraphic :: Control s e -> m ()
  unlinkAudio :: Control s e -> m ()
  stand :: m ()
  wait :: Double -> m ()

newtype System s a = System (ReaderT (Foundation s) IO a) deriving (Functor, Applicative, Monad)

unSystem :: Foundation s -> System s a -> IO a
unSystem f m = unsafeCoerce m f

mkSystem :: (Foundation s -> IO a) -> System s a
mkSystem = unsafeCoerce

runSystem :: WindowMode -> BoundingBox2 -> (forall s. System s a) -> IO (Maybe a)
runSystem mode box m = do
    sys <- G.beginGLFW mode box
    f <- Foundation
        <$> newMVar 0
        <*> pure 44100 -- FIX THIS
        <*> newIORef IM.empty
        <*> newIORef IM.empty
        <*> newIORef IM.empty
        <*> newIORef IM.empty
        <*> newIORef IM.empty
        <*> newMVar 0
        <*> pure sys
        <*> newIORef 60
        <*> newIORef IM.empty
        <*> newEmptyMVar
    let win = G.theWindow sys
    GLFW.setKeyCallback win $ Just $ keyCallback f
    GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback f
    GLFW.setCursorPosCallback win $ Just $ cursorPosCallback f
    GLFW.setScrollCallback win $ Just $ scrollCallback f
    ref <- newEmptyMVar
    _ <- flip forkFinally (either throwIO (putMVar ref)) $ unSystem f m
    PA.with undefined undefined undefined (audioProcess f) $ liftIO $ do
        GLFW.setTime 0
        runGraphic f 0
    G.endGLFW sys
    tryTakeMVar ref

data Foundation s = Foundation
    { newObjectId :: MVar Int
    , sampleRate :: Double
    , cores :: IORef (IM.IntMap (MVar (Object Any (System s))))
    , coreGraphic :: IORef (IM.IntMap Any) -- rely on `invoke`
    , coreAudio :: IORef (IM.IntMap Any)
    , coreKeyboard :: IORef (IM.IntMap Any)
    , coreMouse :: IORef (IM.IntMap Any)
    , theTime :: MVar Double
    , theSystem :: G.System
    , targetFPS :: IORef Double
    , textures :: IORef (IM.IntMap G.Texture)
    , theEnd :: MVar ()
    }

runGraphic :: Foundation s -> Double -> IO ()
runGraphic fo t0 = do
    fps <- readIORef (targetFPS fo)
    let t1 = t0 + 1/fps
    G.beginFrame (theSystem fo)
    pics <- broadcast fo (coreGraphic fo) $ \s -> s (1/fps) -- is it appropriate?
    give (TextureStorage (textures fo)) $ mapM_ runPicture pics
    b <- G.endFrame (theSystem fo)
    
    Just t' <- GLFW.getTime
    threadDelay $ floor $ (t1 - t') * 1000 * 1000

    tryTakeMVar (theEnd fo) >>= \case
        Just _ -> return ()
        _ | b -> putMVar (theEnd fo) ()
          | otherwise -> runGraphic fo t1

v2v2 :: V2 Float -> V 2 Float
v2v2 (V2 x y) = case fromVector $ V.fromList [x, y] of
    Just a -> a
    Nothing -> zero

audioProcess :: Foundation s -> Artery IO (PA.Chunk (V 0 Float)) [V 2 Float]
audioProcess fo = effectful $ \(PA.Chunk n _) -> do
    let dt = fromIntegral n / sampleRate fo
    ws <- broadcast fo (coreAudio fo) $ \s -> s dt n
    return $ fmap v2v2 $ foldr (zipWith (+)) (replicate n zero) ws

push :: Foundation s -> MVar (Object Any (System s)) -> e a -> IO a
push fo mc e = do
    c0 <- takeMVar mc
    (a, c) <- unSystem fo $ runObject c0 (unsafeCoerce e)
    putMVar mc c
    return a

broadcast :: Foundation s
    -> IORef (IM.IntMap Any)
    -> something
    -> IO [a]
broadcast fo rfs e = do
    cs <- readIORef (cores fo)
    fs <- readIORef rfs
    forM (IM.assocs fs) $ \(j, f) -> push fo (cs IM.! j) (unsafeCoerce e (unsafeCoerce f))

keyCallback :: Foundation s -> GLFW.KeyCallback
keyCallback fo _ k _ st _ = void $ broadcast fo (coreKeyboard fo)
    $ \s -> s (toEnum . fromEnum $ k :: Key) (GLFW.KeyState'Released /= st)

mouseButtonCallback :: Foundation s -> GLFW.MouseButtonCallback
mouseButtonCallback fo _ btn st _ = void $ broadcast fo (coreMouse fo)
    $ \(s, _, _) -> s (fromEnum btn) (GLFW.MouseButtonState'Released /= st)

cursorPosCallback :: Foundation s -> GLFW.CursorPosCallback
cursorPosCallback fo _ x y = void $ broadcast fo (coreMouse fo)
    $ \(_, s, _) -> s (V2 x y)

scrollCallback :: Foundation s -> GLFW.ScrollCallback
scrollCallback fo _ x y = void $ broadcast fo (coreMouse fo)
    $ \(_, _, s) -> s (V2 x y)

assimilate :: Control s e -> e a -> e a
assimilate _ = id

instance MonadIO (System s) where
    liftIO m = mkSystem $ const m
    {-# INLINE liftIO #-}

instance (s0 ~ s) => MonadObjective s0 (System s) where
    type Base (System s) = System s
    newtype Control s e = Control Int
    Control i .- e = mkSystem $ \fo -> do
        m <- readIORef $ cores fo
        push fo (m IM.! i) (unsafeCoerce e)
    invoke c = mkSystem $ \fo -> do
        n <- takeMVar $ newObjectId fo
        mc <- newMVar (unsafeCoerce c)
        modifyIORef (cores fo) $ IM.insert n mc
        putMVar (newObjectId fo) (n + 1)
        return (Control n)

instance (s0 ~ s) => MonadSystem s0 (System s) where
    linkGraphic con@(Control i) = mkSystem $ \fo -> modifyIORef (coreGraphic fo)
        $ IM.insert i $ unsafeCoerce $ assimilate con . pullGraphic
    linkAudio con@(Control i) = mkSystem $ \fo -> modifyIORef (coreAudio fo)
        $ IM.insert i $ unsafeCoerce $ (assimilate con .) . pullAudio
    unlinkGraphic (Control i) = mkSystem $ \fo -> modifyIORef (coreGraphic fo) $ IM.delete i
    unlinkAudio (Control i) = mkSystem $ \fo -> modifyIORef (coreAudio fo) $ IM.delete i
    linkKeyboard con@(Control i) = mkSystem
        $ \fo -> modifyIORef (coreKeyboard fo)
        $ IM.insert i (unsafeCoerce $ (assimilate con .) . keyEvent)
    unlinkKeyboard (Control i) = mkSystem $ \fo -> modifyIORef (coreKeyboard fo) $ IM.delete i
    linkMouse con@(Control i) = mkSystem
        $ \fo -> modifyIORef (coreMouse fo) $ IM.insert i (unsafeCoerce
            ( (assimilate con .) . mouseButtonEvent
            , assimilate con . cursorEvent
            , assimilate con . scrollEvent))
    unlinkMouse (Control i) = mkSystem $ \fo -> modifyIORef (coreMouse fo) $ IM.delete i
    wait dt = mkSystem $ \fo -> do
        t0 <- takeMVar (theTime fo)
        Just t <- GLFW.getTime
        threadDelay $ floor $ (t0 - t + dt) * 1000 * 1000
        putMVar (theTime fo) $ t0 + dt
    stand = mkSystem $ \fo -> takeMVar (theEnd fo)

newtype TextureStorage = TextureStorage { getTextureStorage :: IORef (IM.IntMap G.Texture) }

instance Affine IO where
    translate = G.translate
    rotateD = G.rotateD
    rotateR t = let t' = t / pi * 180 in G.rotateD t'
    scale = G.scale

instance (Given TextureStorage) => Picture2D IO where
    bitmap (Bitmap bmp h) = do
        m <- readIORef (getTextureStorage given)
        case IM.lookup h m of
            Just t -> G.drawTexture t
            Nothing -> do
                t <- G.installTexture bmp
                writeIORef (getTextureStorage given) $ IM.insert h t m
                G.drawTexture t
    bitmapOnce (Bitmap bmp _) = do
        t <- G.installTexture bmp
        G.drawTexture t
        G.releaseTexture t

    circle = G.circle
    circleOutline = G.circleOutline
    polygon = G.polygon
    polygonOutline = G.polygonOutline
    line = G.line
    thickness = G.thickness
    color = G.color
    blendMode = G.blendMode
