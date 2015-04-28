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
  , getSlowdown
  , getFPS
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
  -- * Compatibility
  , System
  , runSystem
  , runSystemDefault
) where

import Control.Applicative
import Control.Bool
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad hiding (sequence, sequence_, mapM, mapM_, forM, forM_, msum)
import Control.Monad.IO.Class
import Control.Object
import Data.Audio
import Data.BoundingBox
import Data.Color
import Data.Color.Names
import Data.Traversable as T
import Data.Foldable as F hiding (foldr)
import Data.Typeable
import Data.Graphics
import Data.Graphics as U
import Data.Graphics.Bitmap as Bitmap
import Data.Input.Event
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Int (Int32)
import Data.Reflection
import Foreign (castPtr, sizeOf, with, poke, malloc, free)
import Graphics.GL
import Linear
import qualified Call.Internal.GLFW as G
import qualified Call.Internal.PortAudio as PA
import qualified Codec.Picture as C
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Graphics.UI.GLFW as GLFW
import Unsafe.Coerce
import qualified Criterion.Measurement as Criterion
import Control.Parallel

data WindowMode = Windowed | Resizable | FullScreen deriving (Show, Eq, Ord, Read, Typeable)

{-# DEPRECATED System "Use IO instead" #-}
type System s = IO

{-# DEPRECATED runSystemDefault "Use runCallDefault instead" #-}
runSystemDefault :: (Call => IO a) -> IO (Maybe a)
runSystemDefault = runCallDefault

{-# DEPRECATED runSystem "Use runCall instead" #-}
runSystem :: WindowMode -> Box V2 Float -> (Call => IO a) -> IO (Maybe a)
runSystem = runCall

runCallDefault :: (Call => IO a) -> IO (Maybe a)
runCallDefault = runCall Windowed (Box (V2 0 0) (V2 640 480))

readBitmap :: MonadIO m => FilePath -> m Bitmap.Bitmap
readBitmap = Bitmap.readFile

setFPS :: Call => Float -> IO ()
setFPS f = writeIORef (targetFPS given) f

-- | @(Actual FPS) = (Target FPS) / (1 + (Slowdown))@
getFPS :: Call => IO Float
getFPS = do
  t <- readIORef (targetFPS given)
  r <- getSlowdown
  return $ t / (1 + r)

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
  writeIORef (coreAudio given) $ \mv -> do
    let n = MV.length mv
        dt = fromIntegral n / sampleRate given
    f dt n >>= V.unsafeCopy mv

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
  , coreAudio :: IORef (MV.IOVector Stereo -> IO ())
  , coreKeyboard :: IORef (Chatter Key -> IO ())
  , coreMouse :: IORef (MouseEvent -> IO ())
  , coreJoypad :: IORef (GamepadEvent -> IO ())
  , theTime :: MVar Time
  , theSystem :: G.System
  , targetFPS :: IORef Float
  , textures :: IORef (HM.HashMap Int G.Texture)
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
    <*> newIORef (const $ return ())
    <*> newIORef (const $ return ())
    <*> newIORef (const $ return ())
    <*> newIORef (const $ return ())
    <*> newMVar 0
    <*> pure sys
    <*> newIORef 60
    <*> newIORef HM.empty
    <*> newEmptyMVar
    <*> newIORef IM.empty
    <*> newIORef Map.empty
  let win = G.theWindow sys
  give fd $ do
    GLFW.setKeyCallback win $ Just keyCallback
    GLFW.setMouseButtonCallback win $ Just mouseButtonCallback
    GLFW.setCursorPosCallback win $ Just cursorPosCallback
    GLFW.setScrollCallback win $ Just scrollCallback

  G.withUniform (G.theProgram sys) "color"
    $ \loc -> with (V4 1 1 1 1 :: V4 Float)
    $ \ptr -> glUniform4fv loc 1 (castPtr ptr)

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

  Criterion.initializeTime

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
clearColor (V4 r g b a) = glClearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

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

  bs0' <- forM (ps IM.\\ bs0) $ \p@(Gamepad _ s) -> do
    m $ PadConnection $ Up p
    return (s, IM.empty)

  bs0_ <- iforM (bs0 IM.\\ ps) $ \i (s, _) -> do
    m $ PadConnection $ Down $ Gamepad i s

  let bs1 = bs0 `IM.union` bs0' IM.\\ bs0_

  ls <- iforM ps $ \j p@(Gamepad _ s) -> do
    bs <- zip [0..] <$> gamepadButtons p
    forM_ bs $ \(i, v) -> case (v, maybe False id (bs1 ^? ix j . _2 . ix i)) of
        (False, True) -> m $ PadButton p (Up i)
        (True, False) -> m $ PadButton p (Down i)
        _ -> return ()
    return (s, IM.fromList bs)

  writeIORef (theGamepadButtons given) $ ifoldr IM.insert bs1 ls

getSlowdown :: Call => IO Float
getSlowdown = do
  m <- readIORef (slowdown given)
  return $ F.sum m

runGraphic :: Call => Time -> IO ()
runGraphic t0 = do
  pollGamepad
  fps <- readIORef (targetFPS given)
  G.beginFrame (theSystem given)
  m <- readIORef (coreGraphic given)
  pic <- m (1/fps) -- is it appropriate?
  drawSight pic
  b <- G.endFrame (theSystem given)

  Just t <- fmap (fmap realToFrac) $ GLFW.getTime

  case t0 + 1/fps - realToFrac t of
    dt | dt > 0 -> threadDelay $ floor $ dt * 1000 * 1000
       | otherwise -> modifyIORef' (slowdown given) $ \x -> x
        & at t ?~ negate dt
        & Map.split (t - 1)
        & snd

  tryTakeMVar (theEnd given) >>= \case
      Just _ -> return ()
      _ | b -> putMVar (theEnd given) ()
        | otherwise -> runGraphic (max t (t0 + 1/fps))

audioProcess :: Call => MV.IOVector Stereo -> IO ()
audioProcess mv = do
  m <- readIORef (coreAudio given)
  m mv

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
  case HM.lookup h st of
    Just t -> return $! t
    Nothing -> do
      t <- G.installTexture bmp
      writeIORef (textures given) $ HM.insert h t st
      return t
{-# INLINE fetchTexture #-}

sideEffect :: IO () -> Endo (IO ())
sideEffect m = Endo (m>>)
{-# INLINE sideEffect #-}

runSideEffect :: Endo (IO ()) -> IO ()
runSideEffect (Endo m) = m (return ())
{-# INLINE runSideEffect #-}

mult44 :: M44 Float -> M44 Float -> M44 Float
mult44 f g = fmap (plus . liftA2 (^*) g) f where
  plus (V4 a b c d) = (a ^+^ b) ^+^ (c ^+^ d)
  {-# INLINE plus #-}
{-# INLINE mult44 #-}

drawScene :: Call => Box V2 Float -> M44 Float -> Bool -> Scene -> Endo (IO ())
drawScene (fmap round -> Box (V2 x0 y0) (V2 x1 y1)) proj cull (Scene scene) = sideEffect $ do
  glViewport x0 y0 (x1 - x0) (y1 - y0)
  if cull
    then glCullFace GL_BACK
    else glCullFace GL_FRONT_AND_BACK

  let shaderProg = G.theProgram $ theSystem given

  glUseProgram shaderProg
  G.withUniform shaderProg "projection" $ \loc -> with proj
    $ \ptr -> glUniformMatrix4fv loc 1 1 $ castPtr ptr
  !locTextureMix <- G.withUniform shaderProg "textureMix" return
  !locMat <- G.withUniform shaderProg "model" return
  !locLevel <- G.withUniform shaderProg "level" return
  !locDiffuse <- G.withUniform shaderProg "diffuse" return
  G.withUniform shaderProg "fogDensity" $ \loc -> glUniform1f loc 0
  with (V4 1 1 (1 :: Float) 1) $ \ptr -> glUniform4fv locDiffuse 1 (castPtr ptr)
  glUniform1f locTextureMix 1
  glActiveTexture 0
  pmat <- malloc
  t0 <- Criterion.getTime
  let drawT bmp h (m, buf, n) (_, mat) = sideEffect $ do
        tex <- fetchTexture bmp h
        glBindTexture GL_TEXTURE_2D tex
        -- glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
        glBindBuffer GL_ARRAY_BUFFER buf
        poke pmat mat
        glUniformMatrix4fv locMat 1 1 (castPtr pmat)
        G.vertexAttributes
        glDrawArrays m 0 n
      {-# INLINE drawT #-}
      draw (m, buf, n) (_, mat) = sideEffect $ do
        glUniform1f locTextureMix 0
        glBindBuffer GL_ARRAY_BUFFER buf
        G.vertexAttributes
        glDrawArrays m 0 n
        glUniform1f locTextureMix 1
      {-# INLINE draw #-}
      withMatrix mat r (color0, m) = sideEffect $ do
        let !m' = mult44 m mat
        runSideEffect $ r (color0, m')
      {-# INLINE withMatrix #-}

      go (WithVertices mode vs r) c = sideEffect $ do
        buf <- G.overPtr $ glGenBuffers 1
        let siz = fromIntegral $ V.length vs * sizeOf (undefined :: Vertex)
        glBindBuffer GL_ARRAY_BUFFER buf
        G.vertexAttributes
        V.unsafeWith vs $ \v -> glBufferData GL_ARRAY_BUFFER siz (castPtr v) GL_STATIC_DRAW
        runSideEffect $ r (convPrimitiveMode mode
          , buf
          , fromIntegral $ V.length vs * sizeOf (undefined :: Vertex)) c
        with buf $ glDeleteBuffers 1
      go (EmbedIO m) c = sideEffect $ m >>= runSideEffect . ($ c)
      go (Foggy d color r) c = sideEffect $ do
        G.withUniform shaderProg "fogDensity" $ \loc -> glUniform1f loc d
        with color $ \ptr -> G.withUniform shaderProg "fogColor" $ \loc -> glUniform4fv loc 1 (castPtr ptr)
        runSideEffect $ r c
        G.withUniform shaderProg "fogDensity" $ \loc -> glUniform1f loc 0
      go (Diffuse col r) (color0, n) = sideEffect $ do
        let c = col * color0
        with c $ \ptr -> glUniform4fv locDiffuse 1 (castPtr ptr)
        runSideEffect $ r (c, n)
        with color0 $ \ptr -> glUniform4fv locDiffuse 1 (castPtr ptr)
      {-# INLINE go #-}
  runSideEffect $ runRendering scene (Intensive drawT draw withMatrix) go (V4 1 1 1 1, identity)
  free pmat
  t1 <- Criterion.getTime
  print (t1 - t0)

drawSight :: Call => Sight -> IO ()
drawSight s = do
  b <- readIORef $ G.refRegion $ theSystem given
  runSideEffect $ unSight s b drawScene

convPrimitiveMode :: U.PrimitiveMode -> GLenum
convPrimitiveMode U.LineStrip = GL_LINE_STRIP
convPrimitiveMode U.TriangleFan = GL_TRIANGLE_FAN
convPrimitiveMode U.TriangleStrip = GL_TRIANGLE_STRIP
convPrimitiveMode U.LineLoop = GL_LINE_LOOP
