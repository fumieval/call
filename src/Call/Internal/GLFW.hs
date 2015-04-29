{-# LANGUAGE CPP, ViewPatterns, BangPatterns, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Internal.GLFW
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Call.Internal.GLFW where
import Codec.Picture
import Codec.Picture.RGBA8
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Bool
import Control.Lens
import Control.Monad
import Data.Bits
import Data.BoundingBox
import Data.Graphics.Vertex
import Data.IORef
import Foreign
import Foreign.C (CFloat)
import Foreign.C.String
import Graphics.GL
import Linear
import Paths_call
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified GHC.IO.Encoding as Encoding
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Graphics.GL.Ext.EXT.TextureFilterAnisotropic

data System = System
  { refRegion :: IORef (Box V2 Float)
  , theWindow :: GLFW.Window
  , theProgram :: GLuint
  }

type Texture = GLuint

installTexture :: Image PixelRGBA8 -> IO Texture
installTexture (Image w h vec) = do
  tex <- overPtr (glGenTextures 1)
  glBindTexture GL_TEXTURE_2D tex
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glPixelStorei GL_UNPACK_LSB_FIRST 0
  glPixelStorei GL_UNPACK_SWAP_BYTES 0
  glPixelStorei GL_UNPACK_ROW_LENGTH 0
  glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
  glPixelStorei GL_UNPACK_SKIP_ROWS 0
  glPixelStorei GL_UNPACK_SKIP_PIXELS 0
  glPixelStorei GL_UNPACK_SKIP_IMAGES 0
  glPixelStorei GL_UNPACK_ALIGNMENT 1
  let level = floor $ logBase (2 :: Float) $ fromIntegral (max w h)
  glTexStorage2D GL_TEXTURE_2D level GL_SRGB8 (fromIntegral w) (fromIntegral h)

  when gl_EXT_texture_filter_anisotropic
    $ glTexParameterf GL_TEXTURE_2D GL_TEXTURE_MAX_ANISOTROPY_EXT 8

  V.unsafeWith vec $ glTexSubImage2D GL_TEXTURE_2D 0 0 0 (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE . castPtr

  glGenerateMipmap GL_TEXTURE_2D

  return tex

beginFrame :: System -> IO ()
beginFrame _ = do
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

endFrame :: System -> IO Bool
endFrame sys = do
  -- mapM_ print =<< GL.get GL.errors
  GLFW.swapBuffers $ theWindow sys
  GLFW.pollEvents
  GLFW.windowShouldClose (theWindow sys)

beginGLFW :: Bool -> Bool -> Box V2 Float -> IO System
beginGLFW full resiz bbox@(Box (V2 x0 y0) (V2 x1 y1)) = do
  Encoding.setForeignEncoding Encoding.utf8
  let title = "call"
      ww = floor $ x1 - x0
      wh = floor $ y1 - y0
  () <- unlessM GLFW.init (fail "Failed to initialize")

  mon <- if full then GLFW.getPrimaryMonitor else return Nothing

  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True

  GLFW.windowHint $ GLFW.WindowHint'Resizable resiz
  win <- GLFW.createWindow ww wh title mon Nothing >>= maybe (fail "Failed to create a window") return
  GLFW.makeContextCurrent (Just win)
  prog <- initializeGL

  GLFW.swapInterval 1
  -- GL.clearColor $= GL.Color4 1 1 1 1

  (fw, fh) <- GLFW.getFramebufferSize win

  rbox <- newIORef $ bbox & size zero .~ fmap fromIntegral (V2 fw fh)

  GLFW.setFramebufferSizeCallback win $ Just $ \_ w h -> do
      modifyIORef rbox $ size zero .~ fmap fromIntegral (V2 w h)

  return $ System rbox win prog

compileShader :: FilePath -> GLuint -> IO ()
compileShader path shader = do
  src <- getDataFileName path >>= Text.readFile
  BS.useAsCString (Text.encodeUtf8 src) $ \ptr -> withArray [ptr]
    $ \srcs -> glShaderSource shader 1 srcs nullPtr
  glCompileShader shader

overPtr :: (Storable a) => (Ptr a -> IO b) -> IO a
overPtr f = alloca $ \p -> f p >> peek p
{-# INLINE overPtr #-}

vertexAttributes :: IO ()
vertexAttributes = do
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE stride nullPtr
  glEnableVertexAttribArray 0

  glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE stride pos'
  glEnableVertexAttribArray 1

  glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE stride pos''
  glEnableVertexAttribArray 2
  where
    !stride = fromIntegral $ sizeOf (undefined :: Vertex)
    !pos' = nullPtr `plusPtr` sizeOf (0 :: V3 CFloat)
    !pos'' = pos' `plusPtr` sizeOf (0 :: V2 CFloat)
{-# INLINE vertexAttributes #-}

initializeGL :: IO GLuint
initializeGL = do
  vertexShader <- glCreateShader GL_VERTEX_SHADER
  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  compileShader "shaders/vertex.glsl" vertexShader
  compileShader "shaders/fragment.glsl" fragmentShader
  shaderProg <- glCreateProgram
  glAttachShader shaderProg vertexShader
  glAttachShader shaderProg fragmentShader
  withCString "in_Position" $ glBindAttribLocation shaderProg 0
  withCString "in_UV" $ glBindAttribLocation shaderProg 1
  withCString "in_Normal" $ glBindAttribLocation shaderProg 2
  glLinkProgram shaderProg
  glUseProgram shaderProg

  glDepthMask GL_TRUE
  glDepthFunc GL_LEQUAL
  glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
  glEnable GL_CULL_FACE
  -- glEnable GL_BLEND
  -- glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  glBlendFunc GL_ONE GL_ZERO
  glEnable GL_DEPTH_TEST
  glEnable GL_FRAMEBUFFER_SRGB

  linked <- overPtr (glGetProgramiv shaderProg GL_LINK_STATUS)
  when (linked == GL_FALSE) $ do
    maxLength <- overPtr (glGetProgramiv shaderProg GL_INFO_LOG_LENGTH)
    logLines <- allocaArray (fromIntegral maxLength) $ \p -> alloca $ \lenP -> do
      glGetProgramInfoLog shaderProg maxLength lenP p
      len <- peek lenP
      peekCStringLen (p,fromIntegral len)
    putStrLn logLines

  glEnable GL_LINE_SMOOTH
  glClearColor 0 0 0 1
  withUniform shaderProg "useEnv" $ \l -> glUniform1i l 0
  withUniform shaderProg "tex" $ \l -> glUniform1i l 0
  withUniform shaderProg "env" $ \l -> glUniform1i l 1
  withUniform shaderProg "envAdd" $ \l -> glUniform1f l 0
  withUniform shaderProg "envMul" $ \l -> glUniform1f l 0

  return shaderProg

withUniform :: GLuint -> String -> (GLint -> IO a) -> IO a
withUniform prog str k = withCString str $ \p -> glGetUniformLocation prog p >>= k
{-# INLINE withUniform #-}

endGLFW :: System -> IO ()
endGLFW sys = do
  GLFW.destroyWindow (theWindow sys)
  GLFW.terminate

screenshotFlipped :: System -> IO (Image PixelRGBA8)
screenshotFlipped sys = do
  V2 w h <- fmap floor <$> view (size zero) <$> readIORef (refRegion sys)
  mv <- MV.unsafeNew (w * h * 4)
  glReadBuffer GL_FRONT
  MV.unsafeWith mv $ glReadPixels 0 0 (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE . castPtr

  Image w h <$> V.unsafeFreeze mv

screenshot :: System -> IO (Image PixelRGBA8)
screenshot sys = flipVertically <$> screenshotFlipped sys
