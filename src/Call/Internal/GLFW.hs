{-# LANGUAGE BangPatterns #-}
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
import Control.Bool
import Control.Applicative
import Control.Lens
import Data.IORef
import Data.BoundingBox
import Data.Graphics.Class
import Data.Graphics.Vertex
import Control.Monad
import Graphics.Rendering.OpenGL.GL.StateVar
import Linear
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU.Errors as GL
import qualified Graphics.UI.GLFW as GLFW
import Unsafe.Coerce
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Codec.Picture
import Codec.Picture.RGBA8
import qualified GHC.IO.Encoding as Encoding
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import Foreign.C (CFloat)
import Foreign
import Paths_call
data System = System
  { refRegion :: IORef (Box V2 Float)
  , theWindow :: GLFW.Window
  , theProgram :: GL.Program
  }

type Texture = (GL.TextureObject, Double, Double)

gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf = unsafeCoerce

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei = unsafeCoerce

installTexture :: Image PixelRGBA8 -> IO Texture
installTexture (Image w h v) = do
  [tex] <- GL.genObjectNames 1
  GL.textureBinding GL.Texture2D GL.$= Just tex
  let siz = GL.TextureSize2D (gsizei w) (gsizei h)
  V.unsafeWith v
    $ GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 siz 0
    . GL.PixelData GL.RGBA GL.UnsignedByte
  return (tex, fromIntegral w / 2, fromIntegral h / 2)

releaseTexture :: Texture -> IO ()
releaseTexture (tex, _, _) = GL.deleteObjectNames [tex]

beginFrame :: System -> IO ()
beginFrame _ = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

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

compileShader :: FilePath -> GL.Shader -> IO ()
compileShader path shader = do
  src <- getDataFileName path >>= Text.readFile
  GL.shaderSourceBS shader $= Text.encodeUtf8 src
  GL.compileShader shader
  GL.get (GL.shaderInfoLog shader) >>= putStrLn

initializeGL :: IO GL.Program
initializeGL = do
  let vertexAttribute = GL.AttribLocation 0
  let uvAttribute = GL.AttribLocation 1
  let normalAttribute = GL.AttribLocation 2

  cubeVao <- GL.genObjectName
  cubeVbo <- GL.genObjectName

  GL.bindVertexArrayObject $= Just cubeVao
  GL.bindBuffer GL.ArrayBuffer $= Just cubeVbo

  let stride = fromIntegral $ sizeOf (undefined :: Vertex)

  GL.vertexAttribPointer vertexAttribute $=
    (GL.ToFloat,GL.VertexArrayDescriptor 3 GL.Float stride nullPtr)

  GL.vertexAttribArray vertexAttribute $= GL.Enabled
  GL.bindBuffer GL.ArrayBuffer $= Just cubeVbo

  GL.vertexAttribPointer uvAttribute $=
    (GL.ToFloat
    ,GL.VertexArrayDescriptor 2 GL.Float stride (nullPtr `plusPtr` sizeOf (0 :: V3 CFloat)))
  GL.vertexAttribArray uvAttribute $= GL.Enabled

  GL.vertexAttribPointer normalAttribute $=
    (GL.ToFloat
    ,GL.VertexArrayDescriptor 3 GL.Float stride
      (nullPtr `plusPtr` sizeOf (0 :: V3 CFloat) `plusPtr` sizeOf (0 :: V2 CFloat)))
  GL.vertexAttribArray normalAttribute $= GL.Enabled

  vertexShader <- GL.createShader GL.VertexShader
  fragmentShader <- GL.createShader GL.FragmentShader
  compileShader "shaders/vertex.glsl" vertexShader
  compileShader "shaders/fragment.glsl" fragmentShader
  shaderProg <- GL.createProgram
  GL.attachShader shaderProg vertexShader
  GL.attachShader shaderProg fragmentShader
  GL.attribLocation shaderProg "in_Position" $= vertexAttribute
  GL.attribLocation shaderProg "in_UV" $= uvAttribute
  GL.attribLocation shaderProg "in_Normal" $= normalAttribute
  GL.linkProgram shaderProg
  GL.currentProgram $= Just shaderProg

  GL.depthMask $= GL.Enabled
  GL.depthFunc $= Just GL.Lequal
  GL.colorMask $= GL.Color4 GL.Enabled GL.Enabled GL.Enabled GL.Enabled

  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  linked <- GL.get (GL.linkStatus shaderProg)
  unless linked $ do
    GL.get (GL.programInfoLog shaderProg) >>= putStrLn

  GL.lineSmooth $= GL.Enabled
  GL.textureFunction $= GL.Combine
  GL.clearColor $= GL.Color4 1 1 1 1
  GL.UniformLocation loc0 <- GL.get $ GL.uniformLocation shaderProg "useEnv"
  GL.glUniform1i loc0 0
  GL.UniformLocation loc1 <- GL.get $ GL.uniformLocation shaderProg "tex"
  GL.glUniform1i loc1 0
  GL.UniformLocation loc2 <- GL.get $ GL.uniformLocation shaderProg "env"
  GL.glUniform1i loc2 1
  GL.UniformLocation loc3 <- GL.get $ GL.uniformLocation shaderProg "envAdd"
  GL.glUniform1f loc3 0
  GL.UniformLocation loc4 <- GL.get $ GL.uniformLocation shaderProg "envMul"
  GL.glUniform1f loc4 0

  return shaderProg

endGLFW :: System -> IO ()
endGLFW sys = do
  GLFW.destroyWindow (theWindow sys)
  GLFW.terminate

screenshotFlipped :: System -> IO (Image PixelRGBA8)
screenshotFlipped sys = do
  V2 w h <- fmap floor <$> view (size zero) <$> readIORef (refRegion sys)
  mv <- MV.unsafeNew (w * h * 4)
  GL.readBuffer $= GL.FrontBuffers
  MV.unsafeWith mv
      $ \ptr -> GL.readPixels (GL.Position 0 0) (GL.Size (gsizei w) (gsizei h))
          (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

  Image w h <$> V.unsafeFreeze mv

screenshot :: System -> IO (Image PixelRGBA8)
screenshot sys = flipVertically <$> screenshotFlipped sys

blendMode2BlendingFactors :: BlendMode -> (GL.BlendingFactor, GL.BlendingFactor)
blendMode2BlendingFactors Normal = (GL.SrcAlpha, GL.OneMinusSrcAlpha)
blendMode2BlendingFactors Inverse = (GL.OneMinusDstColor, GL.Zero)
blendMode2BlendingFactors Add = (GL.SrcAlpha, GL.One)
blendMode2BlendingFactors Multiply = (GL.Zero, GL.SrcColor)
blendMode2BlendingFactors Screen = (GL.One, GL.OneMinusSrcColor)

blendMode :: BlendMode -> IO a -> IO a
blendMode mode m = do
  oldFunc <- get GL.blendFunc
  GL.blendFunc $= blendMode2BlendingFactors mode
  a <- m
  GL.blendFunc $= oldFunc
  return a
