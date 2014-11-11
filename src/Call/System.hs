{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
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
module Call.System (
  -- * The system
  System
  , runSystem
  , forkSystem
  , ObjS
  , AddrS
  -- * Time
  , stand
  , wait
  , setFPS
  -- * Raw input
  , keyPress
  , mousePosition
  , mouseButton
  , enableCursor
  , hideCursor
  , disableCursor
  , getGamepads
  , gamepadButtons
  , gamepadAxes
  -- * Component
  , newGraphic
  , newAudio
  , newKeyboard
  , newMouse
  , newJoypad
  , linkGraphic
  , linkAudio
  , linkKeyboard
  , linkMouse
  , linkJoypad
  , unlinkGraphic
  , unlinkAudio
  , unlinkKeyboard
  , unlinkMouse
  , unlinkJoypad) where

import Data.Maybe
import Data.Color
import Control.Lens
import Call.Data.Bitmap
import Call.Sight
import Call.Types
import Call.Event
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Control.Object
import Control.Monad.Objective
import Data.IORef
import Data.Reflection
import Linear
import qualified Call.Internal.GLFW as G
import qualified Data.IntMap.Strict as IM
import qualified Graphics.UI.GLFW as GLFW
import qualified Call.Internal.PortAudio as PA
import Graphics.Rendering.OpenGL.GL.StateVar
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.Rendering.OpenGL.GL as GL
import Unsafe.Coerce
import Foreign (castPtr, sizeOf, with)
import qualified Data.Vector.Storable as V
import Data.BoundingBox (Box(..))

type ObjS e s = Object e (System s)
type AddrS e s = Address e (System s)

newGraphic :: (Lift Graphic e) => ObjS e s -> System s (AddrS e s)
newGraphic o = new o >>= \a -> linkGraphic a >> return a

newAudio :: (Lift Audio e) => ObjS e s -> System s (AddrS e s)
newAudio o = new o >>= \a -> linkAudio a >> return a

newKeyboard :: (Lift Keyboard e) => ObjS e s -> System s (AddrS e s)
newKeyboard o = new o >>= \a -> linkKeyboard a >> return a

newMouse :: (Lift Mouse e) => ObjS e s -> System s (AddrS e s)
newMouse o = new o >>= \a -> linkMouse a >> return a

newJoypad :: (Lift Joypad e) => ObjS e s -> System s (AddrS e s)
newJoypad o = new o >>= \a -> linkJoypad a >> return a

setFPS :: Float -> System s ()
setFPS f = mkSystem $ \fo -> writeIORef (targetFPS fo) f

newtype System s a = System (ReaderT (Foundation s) IO a) deriving (Functor, Applicative, Monad)

unSystem :: Foundation s -> System s a -> IO a
unSystem f m = unsafeCoerce m f

mkSystem :: (Foundation s -> IO a) -> System s a
mkSystem = unsafeCoerce

forkSystem :: System s () -> System s ThreadId
forkSystem m = mkSystem $ \fo -> forkIO (unSystem fo m)

data Foundation s = Foundation
  { newObjectId :: MVar Int
  , sampleRate :: Float
  , coreGraphic :: IORef (IM.IntMap (Member Graphic s))
  , coreAudio :: IORef (IM.IntMap (Member Audio s))
  , coreKeyboard :: IORef (IM.IntMap (Member Keyboard s))
  , coreMouse :: IORef (IM.IntMap (Member Mouse s))
  , coreJoypad :: IORef (IM.IntMap (Member Joypad s))
  , theTime :: MVar Time
  , theSystem :: G.System
  , targetFPS :: IORef Float
  , textures :: IORef (IM.IntMap G.Texture)
  , theEnd :: MVar ()
  , theGamepadButtons :: IORef (IM.IntMap (String, IM.IntMap Bool))
  }

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
    <*> newIORef IM.empty
  let win = G.theWindow sys
  GLFW.setKeyCallback win $ Just $ keyCallback f
  GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback f
  GLFW.setCursorPosCallback win $ Just $ cursorPosCallback f
  GLFW.setScrollCallback win $ Just $ scrollCallback f
  GL.UniformLocation loc <- GL.get $ GL.uniformLocation (G.theProgram sys) "color"
  with (V4 1 1 1 1 :: V4 Float) $ \ptr -> GL.glUniform4fv loc 1 (castPtr ptr)

  print =<< GLFW.getWindowClientAPI win
  putStr "OpenGL Version: "
  cv0 <- GLFW.getWindowContextVersionMajor    win
  cv1 <- GLFW.getWindowContextVersionMinor    win
  cv2 <- GLFW.getWindowContextVersionRevision win
  putStrLn $ show cv0 ++ "." ++ show cv1 ++ "." ++ show cv2 
  print =<< GLFW.getWindowContextRobustness win
  putStr "Forward compat: "
  print =<< GLFW.getWindowOpenGLForwardCompat win
  putStr "Debug context: "
  print =<< GLFW.getWindowOpenGLDebugContext win
  print =<< GLFW.getWindowOpenGLProfile win
  
  ref <- newEmptyMVar
  _ <- flip forkFinally (either throwIO (putMVar ref)) $ unSystem f m
  PA.with 44100 512 (audioProcess f) $ liftIO $ do
    GLFW.setTime 0
    runGraphic f 0
  G.endGLFW sys
  tryTakeMVar ref

linkMouse :: Lift Mouse e => AddrS e s -> System s ()
linkMouse (Control i mc) = mkSystem $ \fo -> modifyIORef (coreMouse fo) $ IM.insert i (Member lift_ mc)

linkKeyboard :: Lift Keyboard e => AddrS e s -> System s ()
linkKeyboard (Control i mc) = mkSystem $ \fo -> modifyIORef (coreKeyboard fo) $ IM.insert i (Member lift_ mc)

linkGraphic :: Lift Graphic e => AddrS e s -> System s ()
linkGraphic (Control i mc) = mkSystem $ \fo -> modifyIORef (coreGraphic fo) $ IM.insert i (Member lift_ mc)

linkAudio :: Lift Audio e => AddrS e s -> System s ()
linkAudio (Control i mc) = mkSystem $ \fo -> modifyIORef (coreAudio fo) $ IM.insert i (Member lift_ mc)

linkJoypad :: Lift Joypad e => AddrS e s -> System s ()
linkJoypad (Control i mc) = mkSystem $ \fo -> modifyIORef (coreJoypad fo) $ IM.insert i (Member lift_ mc)

unlinkMouse :: AddrS e s -> System s ()
unlinkMouse (Control i _) = mkSystem $ \fo -> modifyIORef (coreMouse fo) $ IM.delete i

unlinkKeyboard :: AddrS e s -> System s ()
unlinkKeyboard (Control i _) = mkSystem $ \fo -> modifyIORef (coreKeyboard fo) $ IM.delete i

unlinkGraphic :: AddrS e s -> System s ()
unlinkGraphic (Control i _) = mkSystem $ \fo -> modifyIORef (coreGraphic fo) $ IM.delete i

unlinkAudio :: AddrS e s -> System s ()
unlinkAudio (Control i _) = mkSystem $ \fo -> modifyIORef (coreAudio fo) $ IM.delete i

unlinkJoypad :: AddrS e s -> System s ()
unlinkJoypad (Control i _) = mkSystem $ \fo -> modifyIORef (coreJoypad fo) $ IM.delete i

stand :: System s ()
stand = mkSystem $ \fo -> takeMVar (theEnd fo)

wait :: Time -> System s ()
wait dt = mkSystem $ \fo -> do
  t0 <- takeMVar (theTime fo)
  Just t <- GLFW.getTime
  threadDelay $ floor $ (t0 - realToFrac t + dt) * 1000 * 1000
  putMVar (theTime fo) $ t0 + dt

keyPress :: Key -> System s Bool
keyPress k = mkSystem $ \fo -> fmap (/=GLFW.KeyState'Released)
  $ GLFW.getKey (G.theWindow $ theSystem fo) (toEnum . fromEnum $ k)

mousePosition :: System s (V2 Float)
mousePosition = mkSystem $ \fo -> do
  (x, y) <- GLFW.getCursorPos (G.theWindow $ theSystem fo)
  return $ V2 (realToFrac x) (realToFrac y)

hideCursor :: System s ()
hideCursor = mkSystem $ \fo -> GLFW.setCursorInputMode (G.theWindow $ theSystem fo) GLFW.CursorInputMode'Hidden

disableCursor :: System s ()
disableCursor = mkSystem $ \fo -> GLFW.setCursorInputMode (G.theWindow $ theSystem fo) GLFW.CursorInputMode'Disabled

enableCursor :: System s ()
enableCursor = mkSystem $ \fo -> GLFW.setCursorInputMode (G.theWindow $ theSystem fo) GLFW.CursorInputMode'Normal

mouseButton :: Int -> System s Bool
mouseButton b = mkSystem $ \fo -> fmap (/=GLFW.MouseButtonState'Released)
  $ GLFW.getMouseButton (G.theWindow $ theSystem fo) (toEnum b)

getGamepads :: System s [Gamepad]
getGamepads = mkSystem $ const $ fmap catMaybes $ forM [(GLFW.Joystick'1)..]
  $ \j -> fmap (Gamepad (fromEnum j)) <$> GLFW.getJoystickName j

gamepadAxes :: Gamepad -> System s [Float]
gamepadAxes (Gamepad i _) = mkSystem $ const $ maybe [] (map realToFrac) <$> GLFW.getJoystickAxes (toEnum i)

gamepadButtons :: Gamepad -> System s [Bool]
gamepadButtons (Gamepad i _) = mkSystem $ const
  $ maybe [] (map (==GLFW.JoystickButtonState'Pressed)) <$> GLFW.getJoystickButtons (toEnum i)

data Member e s where
  Member :: (forall x. e x -> f x) -> MVar (Object f (System s)) -> Member e s

instance MonadIO (System s) where
    liftIO m = mkSystem $ const m
    {-# INLINE liftIO #-}

instance MonadObjective (System s) where
  type Residence (System s) = System s
  data Address e (System s) = Control　Int (MVar (Object e (System s)))
  Control _ m .- e = mkSystem $ \fo -> push fo m e
  new c = mkSystem $ \fo -> do
    n <- takeMVar $ newObjectId fo
    mc <- newMVar c
    putMVar (newObjectId fo) (n + 1)
    return (Control n mc)

announce :: Foundation s -> IM.IntMap (Member (Request a ()) s) -> a -> IO ()
announce fo ms r = forM_ (IM.elems ms) $ \(Member e m) -> push fo m $ e $ request r

pollGamepad :: Foundation s -> IO ()
pollGamepad fo = do
  ms <- readIORef (coreJoypad fo)
  ps <- IM.fromList <$> map (\p@(Gamepad i _) -> (i, p)) <$> unSystem fo getGamepads
  bs0 <- readIORef (theGamepadButtons fo)

  bs0' <- forM (IM.toList $ ps IM.\\ bs0) $ \(i, p@(Gamepad _ s)) -> do
    announce fo ms $ PadConnection (Up p)
    return (i, (s, IM.empty))

  bs0_ <- forM (IM.toList $ bs0 IM.\\ ps) $ \(i, (s, _)) -> do
    announce fo ms $ PadConnection (Down $ Gamepad i s)
    return (i, ())

  let bs1 = bs0 `IM.union` IM.fromList bs0' IM.\\ IM.fromList bs0_

  ls <- forM (IM.toList ps) $ \(j, p@(Gamepad _ s)) -> do
    bs <- zip [0..] <$> unSystem fo (gamepadButtons p)
    forM_ bs $ \(i, v) -> case (v, maybe False id (bs1 ^? ix j . _2 . ix i)) of
        (False, True) -> announce fo ms $ PadButton p (Up i)
        (True, False) -> announce fo ms $ PadButton p (Down i)
        _ -> return ()
    return (j, (s, IM.fromList bs))

  writeIORef (theGamepadButtons fo) $ foldr (uncurry IM.insert) bs1 ls

runGraphic :: Foundation s -> Time -> IO ()
runGraphic fo t0 = do
  pollGamepad fo
  fps <- readIORef (targetFPS fo)
  let t1 = t0 + 1/fps
  G.beginFrame (theSystem fo)
  ms <- readIORef (coreGraphic fo)
  pics <- forM (IM.elems ms) $ \(Member e m) -> push fo m $ e $ request (1/fps) -- is it appropriate?
  give (TextureStorage (textures fo)) $ mapM_ (drawSight fo) pics
  b <- G.endFrame (theSystem fo)
  
  Just t' <- GLFW.getTime
  threadDelay $ floor $ (t1 - realToFrac t') * 1000 * 1000

  tryTakeMVar (theEnd fo) >>= \case
      Just _ -> return ()
      _ | b -> putMVar (theEnd fo) ()
        | otherwise -> runGraphic fo t1

audioProcess :: Foundation s -> Int -> IO [V2 Float]
audioProcess fo n = do
  let dt = fromIntegral n / sampleRate fo
  ms <- readIORef (coreAudio fo)
  ws <- forM (IM.elems ms) $ \(Member e m) -> push fo m $ e $ request (dt, n)
  return $ foldr (zipWith (+)) (replicate n zero) ws

push :: Foundation s -> MVar (Object e (System s)) -> e a -> IO a
push fo mc e = do
  c0 <- takeMVar mc
  (a, c) <- unSystem fo $ runObject c0 e
  putMVar mc c
  return a

keyCallback :: Foundation s -> GLFW.KeyCallback
keyCallback fo _ k _ st _ = do
  ms <- readIORef (coreKeyboard fo)
  forM_ (IM.elems ms) $ \(Member e m) -> push fo m
    $ e $ request $ case st of
      GLFW.KeyState'Released -> Up (toEnum . fromEnum $ k :: Key)
      _ -> Down (toEnum . fromEnum $ k :: Key)

mouseButtonCallback :: Foundation s -> GLFW.MouseButtonCallback
mouseButtonCallback fo _ btn st _ = do
  ms <- readIORef (coreMouse fo)
  forM_ (IM.elems ms) $ \(Member e m) -> push fo m
    $ e $ request $ case st of
      GLFW.MouseButtonState'Released -> Button $ Up (fromEnum btn)
      _ -> Button $ Down (fromEnum btn)

cursorPosCallback :: Foundation s -> GLFW.CursorPosCallback
cursorPosCallback fo _ x y = do
  ms <- readIORef (coreMouse fo)
  forM_ (IM.elems ms) $ \(Member e m) -> push fo m $ e $ request $ Cursor $ fmap realToFrac $ V2 x y

scrollCallback :: Foundation s -> GLFW.ScrollCallback
scrollCallback fo _ x y = do
  ms <- readIORef (coreMouse fo)
  forM_ (IM.elems ms) $ \(Member e m) -> push fo m $ e $ request $ Scroll $ fmap realToFrac $ V2 x y

newtype TextureStorage = TextureStorage { getTextureStorage :: IORef (IM.IntMap G.Texture) }

drawScene :: Given TextureStorage => Foundation s -> Box V2 Float -> M44 Float -> Bool -> Scene -> IO ()
drawScene fo (fmap round -> Box (V2 x0 y0) (V2 x1 y1)) proj b (Scene s) = do
  GL.viewport $= (GL.Position x0 y0, GL.Size (x1 - x0) (y1 - y0))

  GL.currentProgram $= Just shaderProg
  GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "projection")
  with proj $ \ptr -> GL.glUniformMatrix4fv loc 1 1 $ castPtr ptr
  GL.UniformLocation locT <- GL.get $ GL.uniformLocation shaderProg "useTexture"
  s (pure $ return ()) (liftA2 (>>)) (prim locT) col trans (RGBA 1 1 1 1, 0)
  where
    shaderProg = G.theProgram $ theSystem fo
    prim locT Blank mode vs _ = do
      GL.glUniform1i locT 0
      V.unsafeWith vs $ \v -> GL.bufferData GL.ArrayBuffer $=
        (fromIntegral $ V.length vs * sizeOf (undefined :: Vertex), v, GL.StaticDraw)
      GL.drawArrays mode 0 $ fromIntegral $ V.length vs
    prim locT (Bitmap bmp _ h) mode vs _ = do
      GL.glUniform1i locT 1
      st <- readIORef (getTextureStorage given)
      (tex, _, _) <- case IM.lookup h st of
        Just t -> return t
        Nothing -> do
          t <- G.installTexture bmp
          writeIORef (getTextureStorage given) $ IM.insert h t st
          return t
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
      GL.textureBinding GL.Texture2D $= Just tex
      V.unsafeWith vs $ \v -> GL.bufferData GL.ArrayBuffer $=
        (fromIntegral $ V.length vs * sizeOf (undefined :: Vertex), v, GL.StaticDraw)
      GL.drawArrays mode 0 $ fromIntegral $ V.length vs
    trans f m (color0, n) = do
      GL.UniformLocation loc <- GL.get $ GL.uniformLocation shaderProg "matrices"
      GL.UniformLocation locN <- GL.get $ GL.uniformLocation shaderProg "level" 
      with f $ \ptr -> GL.glUniformMatrix4fv (loc+n) 1 1 (castPtr ptr)
      GL.glUniform1i locN (unsafeCoerce $ n + 1)
      m (color0, n + 1)
      GL.glUniform1i locN (unsafeCoerce n)
    col f m (color0, n) = do
      GL.UniformLocation loc <- GL.get $ GL.uniformLocation shaderProg "color"
      let c = f color0
      with c $ \ptr -> GL.glUniform4iv loc 1 (castPtr ptr)
      m (c, n)
      with color0 $ \ptr -> GL.glUniform4fv loc 1 (castPtr ptr)      

drawSight :: Given TextureStorage => Foundation s -> Sight -> IO ()
drawSight fo (Sight s) = do
  b <- readIORef $ G.refRegion $ theSystem fo
  s b (return ()) (>>) (drawScene fo)
