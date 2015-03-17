{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
module Call (
  runCallDefault
  , readBitmap
  -- * The system
  , Call
  , Foundation
  , runCall
  , WindowMode(..)
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
    -- * Reexports
  , module Data.Audio
  , module Data.Graphics
  , module Data.Input.Event
  , module Control.Monad
  , module Control.Applicative
  , module Control.Bool
  , module Data.Monoid
  , module Data.Color
  , module Data.Color.Names
  , module Linear
  , module Control.Object
  , module Control.Monad.IO.Class
) where

import Control.Applicative
import Control.Bool
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Object
import Data.Audio
import Data.BoundingBox
import Data.Color
import Data.Color.Names
import Data.Typeable
import Data.Graphics as U
import Data.Graphics
import Data.Graphics.Bitmap as Bitmap
import Data.Input.Event
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Reflection
import Foreign (castPtr, sizeOf, with)
import Graphics.Rendering.OpenGL.GL.StateVar
import Linear
import qualified Call.Internal.GLFW as G
import qualified Call.Internal.PortAudio as PA
import qualified Codec.Picture as C
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.GLFW as GLFW
import Unsafe.Coerce

data WindowMode = Windowed | Resizable | FullScreen deriving (Show, Eq, Ord, Read, Typeable)

runCallDefault :: (Call => IO a) -> IO (Maybe a)
runCallDefault = runCall Windowed (Box (V2 0 0) (V2 640 480))

readBitmap :: MonadIO m => FilePath -> m Bitmap.Bitmap
readBitmap = Bitmap.readFile

setFPS :: Call => Float -> IO ()
setFPS f = writeIORef (targetFPS given) f

type Call = Given Foundation

linkGraphic :: Call => (Time -> IO Sight) -> IO ()
linkGraphic f = do
  g <- readIORef $ coreGraphic given
  writeIORef (coreGraphic given) $ \dt -> liftA2 (<>) (f dt) (g dt)

linkPicture :: Call => (Time -> IO Picture) -> IO ()
linkPicture f = linkGraphic (fmap viewPicture . f)

linkAudio :: Call => (Time -> Int -> IO (V.Vector Stereo)) -> IO ()
linkAudio f = do
  g <- readIORef $ coreAudio given
  writeIORef (coreAudio given) $ \dt n -> liftA2 (V.zipWith (+)) (f dt n) (g dt n)

linkKeyboard :: Call => (Chatter Key -> IO ()) -> IO ()
linkKeyboard f = do
  g <- readIORef $ coreKeyboard given
  writeIORef (coreKeyboard given) $ \k -> f k >> g k

linkMouse :: Call => (MouseEvent -> IO ()) -> IO ()
linkMouse f = do
  g <- readIORef $ coreMouse given
  writeIORef (coreMouse given) $ \k -> f k >> g k

linkGamepad :: Call => (GamepadEvent -> IO ()) -> IO ()
linkGamepad f = do
  g <- readIORef $ coreJoypad given
  writeIORef (coreJoypad given) $ \k -> f k >> g k

data Foundation = Foundation
  { sampleRate :: Float
  , coreGraphic :: IORef (Time -> IO Sight)
  , coreAudio :: IORef (Time -> Int -> IO (V.Vector (V2 Float)))
  , coreKeyboard :: IORef (Chatter Key -> IO ())
  , coreMouse :: IORef (MouseEvent -> IO ())
  , coreJoypad :: IORef (GamepadEvent -> IO ())
  , theTime :: MVar Time
  , theSystem :: G.System
  , targetFPS :: IORef Float
  , textures :: IORef (IM.IntMap G.Texture)
  , theEnd :: MVar ()
  , theGamepadButtons :: IORef (IM.IntMap (String, IM.IntMap Bool))
  , slowdown :: IORef (Map.Map Time Time)
  }

runCall :: WindowMode -> Box V2 Float -> (Call => IO a) -> IO (Maybe a)
runCall mode box m = do
  sys <- G.beginGLFW (mode == FullScreen) (mode == Resizable) box
  fd <- Foundation
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
    <*> newIORef Map.empty
  let win = G.theWindow sys
  give fd $ do
    GLFW.setKeyCallback win $ Just keyCallback
    GLFW.setMouseButtonCallback win $ Just mouseButtonCallback
    GLFW.setCursorPosCallback win $ Just cursorPosCallback
    GLFW.setScrollCallback win $ Just scrollCallback
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
  _ <- flip forkFinally (either throwIO (putMVar ref)) (give fd m)
  give fd $ PA.with 44100 512 audioProcess $ liftIO $ do
    GLFW.setTime 0
    runGraphic 0
  G.endGLFW sys
  tryTakeMVar ref

stand :: Call => IO ()
stand = takeMVar (theEnd given)

wait :: Call => Time -> IO ()
wait dt = do
  t0 <- takeMVar (theTime given)
  Just t <- GLFW.getTime
  threadDelay $ floor $ (t0 - realToFrac t + dt) * 1000 * 1000
  putMVar (theTime given) $ t0 + dt

getTime :: Call => IO Time
getTime = readMVar (theTime given)

keyPress :: Call => Key -> IO Bool
keyPress k = fmap (/=GLFW.KeyState'Released)
  $ GLFW.getKey (G.theWindow $ theSystem given) (toEnum . fromEnum $ k)

mousePosition :: Call => IO (V2 Float)
mousePosition = do
  (x, y) <- GLFW.getCursorPos (G.theWindow $ theSystem given)
  return $ V2 (realToFrac x) (realToFrac y)

hideCursor :: Call => IO ()
hideCursor = GLFW.setCursorInputMode (G.theWindow $ theSystem given) GLFW.CursorInputMode'Hidden

disableCursor :: Call => IO ()
disableCursor = GLFW.setCursorInputMode (G.theWindow $ theSystem given) GLFW.CursorInputMode'Disabled

enableCursor :: Call => IO ()
enableCursor = GLFW.setCursorInputMode (G.theWindow $ theSystem given) GLFW.CursorInputMode'Normal

mouseButton :: Int -> Call => IO Bool
mouseButton b = fmap (/=GLFW.MouseButtonState'Released)
  $ GLFW.getMouseButton (G.theWindow $ theSystem given) (toEnum b)

getGamepads :: Call => IO [Gamepad]
getGamepads = fmap catMaybes $ forM [(GLFW.Joystick'1)..]
  $ \j -> fmap (Gamepad (fromEnum j)) <$> GLFW.getJoystickName j

gamepadAxes :: Call => Gamepad -> IO [Float]
gamepadAxes (Gamepad i _) = maybe [] (map realToFrac) <$> GLFW.getJoystickAxes (toEnum i)

gamepadButtons :: Call => Gamepad -> IO [Bool]
gamepadButtons (Gamepad i _) = maybe [] (map (==GLFW.JoystickButtonState'Pressed)) <$> GLFW.getJoystickButtons (toEnum i)

clearColor :: Call => V4 Float -> IO ()
clearColor (V4 r g b a) = liftIO $ GL.clearColor $= GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

setBoundingBox :: Call => Box V2 Float -> IO ()
setBoundingBox box@(Box (V2 x0 y0) (V2 x1 y1)) = do
  GLFW.setWindowSize (G.theWindow $ theSystem given) (floor (x1 - x0)) (floor (y1 - y0))
  writeIORef (G.refRegion $ theSystem given) box

getBoundingBox :: Call => IO (Box V2 Float)
getBoundingBox = readIORef (G.refRegion $ theSystem given)

takeScreenshot :: Call => IO Bitmap
takeScreenshot = G.screenshot (theSystem given) >>= liftImage'

setTitle :: Call => String -> IO ()
setTitle str = GLFW.setWindowTitle (G.theWindow $ theSystem given) str

pollGamepad :: Call => IO ()
pollGamepad = do
  m <- readIORef (coreJoypad given)
  ps <- IM.fromList <$> map (\p@(Gamepad i _) -> (i, p)) <$> getGamepads
  bs0 <- readIORef (theGamepadButtons given)

  bs0' <- forM (IM.toList $ ps IM.\\ bs0) $ \(i, p@(Gamepad _ s)) -> do
    m $ PadConnection $ Up p
    return (i, (s, IM.empty))

  bs0_ <- forM (IM.toList $ bs0 IM.\\ ps) $ \(i, (s, _)) -> do
    m $ PadConnection $ Down $ Gamepad i s
    return (i, ())

  let bs1 = bs0 `IM.union` IM.fromList bs0' IM.\\ IM.fromList bs0_

  ls <- forM (IM.toList ps) $ \(j, p@(Gamepad _ s)) -> do
    bs <- zip [0..] <$> gamepadButtons p
    forM_ bs $ \(i, v) -> case (v, maybe False id (bs1 ^? ix j . _2 . ix i)) of
        (False, True) -> m $ PadButton p (Up i)
        (True, False) -> m $ PadButton p (Down i)
        _ -> return ()
    return (j, (s, IM.fromList bs))

  writeIORef (theGamepadButtons given) $ foldr (uncurry IM.insert) bs1 ls

runGraphic :: Call => Time -> IO ()
runGraphic t0 = do
  pollGamepad
  fps <- readIORef (targetFPS given)
  G.beginFrame (theSystem given)
  m <- readIORef (coreGraphic given)
  pic <- m (1/fps) -- is it appropriate?
  drawSight pic
  b <- G.endFrame (theSystem given)

  Just t <- GLFW.getTime

  case t0 + 1/fps - realToFrac t of
    dt | dt > 0 -> threadDelay $ floor $ dt * 1000 * 1000
       | otherwise -> return () -- modifyIORef_ (slowdown given) $ at t ?~ negate dt

  tryTakeMVar (theEnd given) >>= \case
      Just _ -> return ()
      _ | b -> putMVar (theEnd given) ()
        | otherwise -> runGraphic (t0 + 1 / fps)

audioProcess :: Call => Int -> IO (V.Vector Stereo)
audioProcess n = do
  let dt = fromIntegral n / sampleRate given
  m <- readIORef (coreAudio given)
  m dt n

keyCallback :: Call => GLFW.KeyCallback
keyCallback _ k _ st _ = do
  m <- readIORef (coreKeyboard given)
  m $ case st of
    GLFW.KeyState'Released -> Up (toEnum . fromEnum $ k :: Key)
    _ -> Down (toEnum . fromEnum $ k :: Key)

mouseButtonCallback :: Call => GLFW.MouseButtonCallback
mouseButtonCallback _ btn st _ = do
  m <- readIORef (coreMouse given)
  m $ case st of
    GLFW.MouseButtonState'Released -> Button $ Up (fromEnum btn)
    _ -> Button $ Down (fromEnum btn)

cursorPosCallback :: Call => GLFW.CursorPosCallback
cursorPosCallback _ x y = do
  m <- readIORef (coreMouse given)
  m $ Cursor $ fmap realToFrac $ V2 x y

scrollCallback :: Call => GLFW.ScrollCallback
scrollCallback _ x y = do
  m <- readIORef (coreMouse given)
  m $ Scroll $ fmap realToFrac $ V2 x y

fetchTexture :: Call => C.Image C.PixelRGBA8 -> Int -> IO G.Texture
fetchTexture bmp h = do
  st <- readIORef (textures given)
  case IM.lookup h st of
    Just t -> return t
    Nothing -> do
      t <- G.installTexture bmp
      writeIORef (textures given) $ IM.insert h t st
      return t

drawScene :: Call => Box V2 Float -> M44 Float -> Bool -> Scene -> IO ()
drawScene (fmap round -> Box (V2 x0 y0) (V2 x1 y1)) proj _ (Scene s) = do
  GL.viewport $= (GL.Position x0 y0, GL.Size (x1 - x0) (y1 - y0))

  GL.currentProgram $= Just shaderProg
  GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "projection")
  with proj $ \ptr -> GL.glUniformMatrix4fv loc 1 1 $ castPtr ptr
  GL.UniformLocation locTextureMix <- GL.get $ GL.uniformLocation shaderProg "textureMix"
  GL.UniformLocation locMats <- GL.get $ GL.uniformLocation shaderProg "matrices"
  GL.UniformLocation locLevel <- GL.get $ GL.uniformLocation shaderProg "level"
  GL.UniformLocation locDiffuse <- GL.get $ GL.uniformLocation shaderProg "diffuse"
  with (V4 1 1 (1 :: Float) 1) $ \ptr -> GL.glUniform4fv locDiffuse 1 (castPtr ptr)
  s (pure $ return ()) (liftA2 (>>)) (prim locTextureMix) (fx locDiffuse) (trans locMats locLevel) (V4 1 1 1 1, 0)
  where
    shaderProg = G.theProgram $ theSystem given
    prim locTextureMix Blank mode vs _ = do
      GL.glUniform1f locTextureMix 0
      V.unsafeWith vs $ \v -> GL.bufferData GL.ArrayBuffer $=
        (fromIntegral $ V.length vs * sizeOf (undefined :: Vertex), v, GL.StaticDraw)
      GL.drawArrays (convPrimitiveMode mode) 0 $ fromIntegral $ V.length vs

    prim locTextureMix (Bitmap bmp _ h) mode vs _ = do
      GL.glUniform1f locTextureMix 1
      (tex, _, _) <- fetchTexture bmp h
      GL.activeTexture $= GL.TextureUnit 0
      GL.textureBinding GL.Texture2D $= Just tex
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
      V.unsafeWith vs $ \v -> GL.bufferData GL.ArrayBuffer $=
        (fromIntegral $ V.length vs * sizeOf (undefined :: Vertex), v, GL.StaticDraw)
      GL.drawArrays (convPrimitiveMode mode) 0 $ fromIntegral $ V.length vs

    trans locMats locLevel f m (color0, n) = do
      with f $ \ptr -> GL.glUniformMatrix4fv (locMats + n) 1 1 (castPtr ptr)
      GL.glUniform1i locLevel (unsafeCoerce $ n + 1)
      () <- m (color0, n + 1)
      GL.glUniform1i locLevel (unsafeCoerce n)

    fx locDiffuse (EmbedIO m) c = m >>= ($ c)

    fx locDiffuse (Diffuse col m) (color0, n) = do
      let c = col * color0
      with c $ \ptr -> GL.glUniform4fv locDiffuse 1 (castPtr ptr)
      m (c, n)
      with color0 $ \ptr -> GL.glUniform4fv locDiffuse 1 (castPtr ptr)

drawSight :: Call => Sight -> IO ()
drawSight (Sight s) = do
  b <- readIORef $ G.refRegion $ theSystem given
  s b (return ()) (>>) drawScene

convPrimitiveMode :: U.PrimitiveMode -> GL.PrimitiveMode
convPrimitiveMode U.LineStrip = GL.LineStrip
convPrimitiveMode U.TriangleFan = GL.TriangleFan
convPrimitiveMode U.TriangleStrip = GL.TriangleStrip
convPrimitiveMode U.LineLoop = GL.LineLoop
