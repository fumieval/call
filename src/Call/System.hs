{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
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

data Member c s = forall e. c e => Member (MVar (Object e (System s)))

data Foundation s = Foundation
    { newObjectId :: MVar Int
    , sampleRate :: Double
    , coreGraphic :: IORef (IM.IntMap (Member Graphic s))
    , coreAudio :: IORef (IM.IntMap (Member Audio s))
    , coreKeyboard :: IORef (IM.IntMap (Member HandleKeyboard s))
    , coreMouse :: IORef (IM.IntMap (Member HandleMouse s))
    , theTime :: MVar Double
    , theSystem :: G.System
    , targetFPS :: IORef Double
    , textures :: IORef (IM.IntMap G.Texture)
    , theEnd :: MVar ()
    }

instance MonadIO (System s) where
    liftIO m = mkSystem $ const m
    {-# INLINE liftIO #-}

instance (s0 ~ s) => MonadObjective s0 (System s) where
    type Base (System s) = System s
    data Control s e = Control　Int (MVar (Object e (System s)))
    Control _ m .- e = mkSystem $ \fo -> push fo m e
    invoke c = mkSystem $ \fo -> do
        n <- takeMVar $ newObjectId fo
        mc <- newMVar c
        putMVar (newObjectId fo) (n + 1)
        return (Control n mc)

instance (s0 ~ s) => MonadSystem s0 (System s) where
    linkGraphic (Control i mc) = mkSystem $ \fo -> modifyIORef (coreGraphic fo) $ IM.insert i (Member mc)
    linkAudio (Control i mc) = mkSystem $ \fo -> modifyIORef (coreAudio fo) $ IM.insert i (Member mc)
    linkKeyboard (Control i mc) = mkSystem $ \fo -> modifyIORef (coreKeyboard fo) $ IM.insert i (Member mc)
    linkMouse (Control i mc) = mkSystem $ \fo -> modifyIORef (coreMouse fo) $ IM.insert i (Member mc)
    unlinkGraphic (Control i _) = mkSystem $ \fo -> modifyIORef (coreGraphic fo) $ IM.delete i
    unlinkAudio (Control i _) = mkSystem $ \fo -> modifyIORef (coreAudio fo) $ IM.delete i
    unlinkMouse (Control i _) = mkSystem $ \fo -> modifyIORef (coreMouse fo) $ IM.delete i
    unlinkKeyboard (Control i _) = mkSystem $ \fo -> modifyIORef (coreKeyboard fo) $ IM.delete i

    wait dt = mkSystem $ \fo -> do
        t0 <- takeMVar (theTime fo)
        Just t <- GLFW.getTime
        threadDelay $ floor $ (t0 - t + dt) * 1000 * 1000
        putMVar (theTime fo) $ t0 + dt
    stand = mkSystem $ \fo -> takeMVar (theEnd fo)

runGraphic :: Foundation s -> Double -> IO ()
runGraphic fo t0 = do
    fps <- readIORef (targetFPS fo)
    let t1 = t0 + 1/fps
    G.beginFrame (theSystem fo)
    ms <- readIORef (coreGraphic fo)
    pics <- forM (IM.elems ms) $ \(Member m) -> push fo m $ pullGraphic (1/fps) -- is it appropriate?
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
    ms <- readIORef (coreAudio fo)
    ws <- forM (IM.elems ms) $ \(Member m) -> push fo m $ pullAudio dt n
    return $ fmap v2v2 $ foldr (zipWith (+)) (replicate n zero) ws

push :: Foundation s -> MVar (Object e (System s)) -> e a -> IO a
push fo mc e = do
    c0 <- takeMVar mc
    (a, c) <- unSystem fo $ runObject c0 e
    putMVar mc c
    return a

keyCallback :: Foundation s -> GLFW.KeyCallback
keyCallback fo _ k _ st _ = do
    ms <- readIORef (coreKeyboard fo)
    forM_ (IM.elems ms) $ \(Member m) -> push fo m
        $ keyEvent (toEnum . fromEnum $ k :: Key) (GLFW.KeyState'Released /= st)

mouseButtonCallback :: Foundation s -> GLFW.MouseButtonCallback
mouseButtonCallback fo _ btn st _ = do
    ms <- readIORef (coreMouse fo)
    forM_ (IM.elems ms) $ \(Member m) -> push fo m
        $ mouseButtonEvent (fromEnum btn) (GLFW.MouseButtonState'Released /= st)

cursorPosCallback :: Foundation s -> GLFW.CursorPosCallback
cursorPosCallback fo _ x y = do
    ms <- readIORef (coreMouse fo)
    forM_ (IM.elems ms) $ \(Member m) -> push fo m $ cursorEvent (V2 x y)

scrollCallback :: Foundation s -> GLFW.ScrollCallback
scrollCallback fo _ x y = do
    ms <- readIORef (coreMouse fo)
    forM_ (IM.elems ms) $ \(Member m) -> push fo m $ scrollEvent (V2 x y)

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
