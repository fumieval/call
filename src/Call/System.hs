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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
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
  -- * Time
  , stand
  , wait
  , getTime
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
  , linkGraphic
  , linkPicture
  , linkAudio
  , linkKeyboard
  , linkMouse
  , linkGamepad
  -- * Others
  , setTitle
  , clearColor
  , getBoundingBox
  , setBoundingBox
  , takeScreenshot
  ) where

import Call.Data.Bitmap
import Call.Sight
import Call.Types
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Elevator
import Control.Lens
import Control.Monad.Objective
import Control.Monad.Reader
import Control.Object
import Data.BoundingBox (Box(..))
import Data.Color
import Data.OpenUnion1.Clean
import Data.IORef
import Data.Maybe
import Data.Monoid
import Foreign (castPtr, sizeOf, with)
import Graphics.Rendering.OpenGL.GL.StateVar
import Linear
import qualified Call.Internal.GLFW as G
import qualified Call.Internal.PortAudio as PA
import qualified Codec.Picture as C
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.GLFW as GLFW
import Unsafe.Coerce

setFPS :: Float -> System s ()
setFPS f = mkSystem $ \fo -> writeIORef (targetFPS fo) f

newtype System s a = System (ReaderT (Foundation s) IO a) deriving (Functor, Applicative, Monad)

instance MonadObjective (System s) where
  data Instance e m (System s) = InstanceS (MVar (Object e m))
  InstanceS m `invoke` e = do
    c <- liftIO $ takeMVar m
    return $ do
      (a, c') <- runObject c e
      return (liftIO (putMVar m c') >> return a)
  new v = liftIO $ InstanceS `fmap` newMVar v

instance Tower (System s) where
  type Floors (System s) = IO :> Empty
  toLoft = liftIO ||> exhaust

unSystem :: Foundation s -> System s a -> IO a
unSystem f m = unsafeCoerce m f

mkSystem :: (Foundation s -> IO a) -> System s a
mkSystem = unsafeCoerce

forkSystem :: System s () -> System s ThreadId
forkSystem m = mkSystem $ \fo -> forkIO (unSystem fo m)

linkGraphic :: (Time -> System s Sight) -> System s ()
linkGraphic f = mkSystem $ \fo -> do
  g <- readIORef $ coreGraphic fo
  writeIORef (coreGraphic fo) $ \dt -> liftA2 (<>) (f dt) (g dt)

linkPicture :: (Time -> System s Picture) -> System s ()
linkPicture f = linkGraphic (fmap viewPicture . f)

linkAudio :: (Time -> Int -> System s (V.Vector Stereo)) -> System s ()
linkAudio f = mkSystem $ \fo -> do
  g <- readIORef $ coreAudio fo
  writeIORef (coreAudio fo) $ \dt n -> liftA2 (V.zipWith (+)) (f dt n) (g dt n)

linkKeyboard :: (Chatter Key -> System s ()) -> System s ()
linkKeyboard f = mkSystem $ \fo -> do
  g <- readIORef $ coreKeyboard fo
  writeIORef (coreKeyboard fo) $ \k -> f k >> g k

linkMouse :: (MouseEvent -> System s ()) -> System s ()
linkMouse f = mkSystem $ \fo -> do
  g <- readIORef $ coreMouse fo
  writeIORef (coreMouse fo) $ \k -> f k >> g k

linkGamepad :: (GamepadEvent -> System s ()) -> System s ()
linkGamepad f = mkSystem $ \fo -> do
  g <- readIORef $ coreJoypad fo
  writeIORef (coreJoypad fo) $ \k -> f k >> g k

data Foundation s = Foundation
  { sampleRate :: Float
  , coreGraphic :: IORef (Time -> System s Sight)
  , coreAudio :: IORef (Time -> Int -> System s (V.Vector (V2 Float)))
  , coreKeyboard :: IORef (Chatter Key -> System s ())
  , coreMouse :: IORef (MouseEvent -> System s ())
  , coreJoypad :: IORef (GamepadEvent -> System s ())
  , theTime :: MVar Time
  , theSystem :: G.System
  , targetFPS :: IORef Float
  , textures :: IORef (IM.IntMap G.Texture)
  , theEnd :: MVar ()
  , theGamepadButtons :: IORef (IM.IntMap (String, IM.IntMap Bool))
  }

runSystem :: WindowMode -> Box V2 Float -> (forall s. System s a) -> IO (Maybe a)
runSystem mode box m = do
  sys <- G.beginGLFW mode box
  f <- Foundation
    <$> pure 44100 -- FIX THIS
    <*> newIORef (const $ return mempty)
    <*> newIORef (\_ n -> return $ V.replicate n zero)
    <*> newIORef (const $ return ())
    <*> newIORef (const $ return ())
    <*> newIORef (const $ return ())
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

stand :: System s ()
stand = mkSystem $ \fo -> takeMVar (theEnd fo)

wait :: Time -> System s ()
wait dt = mkSystem $ \fo -> do
  t0 <- takeMVar (theTime fo)
  Just t <- GLFW.getTime
  threadDelay $ floor $ (t0 - realToFrac t + dt) * 1000 * 1000
  putMVar (theTime fo) $ t0 + dt

getTime :: System s Time
getTime = mkSystem $ \fo -> readMVar (theTime fo)

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

clearColor :: RGBA -> System s ()
clearColor col = liftIO $ GL.clearColor $= unsafeCoerce col

setBoundingBox :: Box V2 Float -> System s ()
setBoundingBox box@(Box (V2 x0 y0) (V2 x1 y1)) = mkSystem $ \fo -> do
  GLFW.setWindowSize (G.theWindow $ theSystem fo) (floor (x1 - x0)) (floor (y1 - y0))
  writeIORef (G.refRegion $ theSystem fo) box

getBoundingBox :: System s (Box V2 Float)
getBoundingBox = mkSystem $ \fo -> readIORef (G.refRegion $ theSystem fo)

takeScreenshot :: System s Bitmap
takeScreenshot = mkSystem $ \fo -> G.screenshot (theSystem fo) >>= liftImage'

setTitle :: String -> System s ()
setTitle str = mkSystem $ \fo -> GLFW.setWindowTitle (G.theWindow $ theSystem fo) str
    
instance MonadIO (System s) where
  liftIO m = mkSystem $ const m
  {-# INLINE liftIO #-}

pollGamepad :: Foundation s -> IO ()
pollGamepad fo = do
  m <- readIORef (coreJoypad fo)
  ps <- IM.fromList <$> map (\p@(Gamepad i _) -> (i, p)) <$> unSystem fo getGamepads
  bs0 <- readIORef (theGamepadButtons fo)

  bs0' <- forM (IM.toList $ ps IM.\\ bs0) $ \(i, p@(Gamepad _ s)) -> do
    unSystem fo $ m $ PadConnection $ Up p
    return (i, (s, IM.empty))

  bs0_ <- forM (IM.toList $ bs0 IM.\\ ps) $ \(i, (s, _)) -> do
    unSystem fo $ m $ PadConnection $ Down $ Gamepad i s
    return (i, ())

  let bs1 = bs0 `IM.union` IM.fromList bs0' IM.\\ IM.fromList bs0_

  ls <- forM (IM.toList ps) $ \(j, p@(Gamepad _ s)) -> do
    bs <- zip [0..] <$> unSystem fo (gamepadButtons p)
    forM_ bs $ \(i, v) -> case (v, maybe False id (bs1 ^? ix j . _2 . ix i)) of
        (False, True) -> unSystem fo $ m $ PadButton p (Up i)
        (True, False) -> unSystem fo $ m $ PadButton p (Down i)
        _ -> return ()
    return (j, (s, IM.fromList bs))

  writeIORef (theGamepadButtons fo) $ foldr (uncurry IM.insert) bs1 ls

runGraphic :: Foundation s -> Time -> IO ()
runGraphic fo t0 = do
  pollGamepad fo
  fps <- readIORef (targetFPS fo)
  let t1 = t0 + 1/fps
  G.beginFrame (theSystem fo)
  m <- readIORef (coreGraphic fo)
  pic <- unSystem fo $ m (1/fps) -- is it appropriate?
  drawSight fo pic
  b <- G.endFrame (theSystem fo)
  
  Just t' <- GLFW.getTime
  threadDelay $ floor $ (t1 - realToFrac t') * 1000 * 1000

  tryTakeMVar (theEnd fo) >>= \case
      Just _ -> return ()
      _ | b -> putMVar (theEnd fo) ()
        | otherwise -> runGraphic fo t1

audioProcess :: Foundation s -> Int -> IO (V.Vector Stereo)
audioProcess fo n = do
  let dt = fromIntegral n / sampleRate fo
  m <- readIORef (coreAudio fo)
  unSystem fo $ m dt n

keyCallback :: Foundation s -> GLFW.KeyCallback
keyCallback fo _ k _ st _ = do
  m <- readIORef (coreKeyboard fo)
  unSystem fo $ m $ case st of
    GLFW.KeyState'Released -> Up (toEnum . fromEnum $ k :: Key)
    _ -> Down (toEnum . fromEnum $ k :: Key)

mouseButtonCallback :: Foundation s -> GLFW.MouseButtonCallback
mouseButtonCallback fo _ btn st _ = do
  m <- readIORef (coreMouse fo)
  unSystem fo $ m $ case st of
    GLFW.MouseButtonState'Released -> Button $ Up (fromEnum btn)
    _ -> Button $ Down (fromEnum btn)

cursorPosCallback :: Foundation s -> GLFW.CursorPosCallback
cursorPosCallback fo _ x y = do
  m <- readIORef (coreMouse fo)
  unSystem fo $ m $ Cursor $ fmap realToFrac $ V2 x y

scrollCallback :: Foundation s -> GLFW.ScrollCallback
scrollCallback fo _ x y = do
  m <- readIORef (coreMouse fo)
  unSystem fo $ m $ Scroll $ fmap realToFrac $ V2 x y

fetchTexture :: Foundation s -> C.Image C.PixelRGBA8 -> Int -> IO G.Texture
fetchTexture fo bmp h = do
  st <- readIORef (textures fo)
  case IM.lookup h st of
    Just t -> return t
    Nothing -> do
      t <- G.installTexture bmp
      writeIORef (textures fo) $ IM.insert h t st
      return t

drawScene :: Foundation s -> Box V2 Float -> M44 Float -> Bool -> Scene -> IO ()
drawScene fo (fmap round -> Box (V2 x0 y0) (V2 x1 y1)) proj _ (Scene s) = do
  GL.viewport $= (GL.Position x0 y0, GL.Size (x1 - x0) (y1 - y0))

  GL.currentProgram $= Just shaderProg
  GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "projection")
  with proj $ \ptr -> GL.glUniformMatrix4fv loc 1 1 $ castPtr ptr
  GL.UniformLocation locT <- GL.get $ GL.uniformLocation shaderProg "textureMix"
  s (pure $ return ()) (liftA2 (>>)) (prim locT) fx trans (RGBA 1 1 1 1, 0)
  where
    shaderProg = G.theProgram $ theSystem fo
    prim locT Blank mode vs _ = do
      GL.glUniform1f locT 0
      V.unsafeWith vs $ \v -> GL.bufferData GL.ArrayBuffer $=
        (fromIntegral $ V.length vs * sizeOf (undefined :: Vertex), v, GL.StaticDraw)
      GL.drawArrays mode 0 $ fromIntegral $ V.length vs
    prim locT (Bitmap bmp _ h) mode vs _ = do
      GL.glUniform1f locT 1
      (tex, _, _) <- fetchTexture fo bmp h
      GL.activeTexture $= GL.TextureUnit 0
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
    fx (Diffuse col m) (color0, n) = do
      GL.UniformLocation loc <- GL.get $ GL.uniformLocation shaderProg "diffuse"
      let c = multRGBA col color0
      with c $ \ptr -> GL.glUniform4fv loc 1 (castPtr ptr)
      m (c, n)
      with color0 $ \ptr -> GL.glUniform4fv loc 1 (castPtr ptr)
    fx (SphericalAdd (Bitmap bmp _ h) m) c = do
      withLoc "useEnv" $ \loc -> GL.glUniform1i loc 1
      withLoc "envAdd" $ \loc -> GL.glUniform1f loc 1
      (tex, _, _) <- fetchTexture fo bmp h
      GL.activeTexture $= GL.TextureUnit 1
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
      GL.textureBinding GL.Texture2D $= Just tex
      m c
      withLoc "useEnv" $ \loc -> GL.glUniform1i loc 0
      withLoc "envAdd" $ \loc -> GL.glUniform1f loc 0
    fx (SphericalAdd Blank m) c = m c
    fx (SphericalMultiply (Bitmap bmp _ h) m) c = do
      withLoc "useEnv" $ \loc -> GL.glUniform1i loc 1
      withLoc "envMul" $ \loc -> GL.glUniform1f loc 1
      (tex, _, _) <- fetchTexture fo bmp h

      GL.activeTexture $= GL.TextureUnit 1
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
      GL.textureBinding GL.Texture2D $= Just tex
      m c
      withLoc "useEnv" $ \loc -> GL.glUniform1i loc 0
      withLoc "envMul" $ \loc -> GL.glUniform1f loc 0
    fx (SphericalMultiply Blank m) c = m c
    fx (NormalMap (Bitmap bmp _ h) m) c = m c
    withLoc s m = do
      GL.UniformLocation loc <- GL.get $ GL.uniformLocation shaderProg "useEnv"
      m loc

drawSight :: Foundation s -> Sight -> IO ()
drawSight fo (Sight s) = do
  b <- readIORef $ G.refRegion $ theSystem fo
  s b (return ()) (>>) (drawScene fo)
