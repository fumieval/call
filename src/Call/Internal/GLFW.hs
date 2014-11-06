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
import Control.Monad.IO.Class
import Control.Lens
import Data.IORef
import Call.Types
import Data.BoundingBox
import Control.Monad
import Graphics.Rendering.OpenGL.GL.StateVar
import Linear
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Unsafe.Coerce
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Codec.Picture
import Codec.Picture.RGBA8
import qualified GHC.IO.Encoding as Encoding
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign.C (CFloat)
import Foreign (nullPtr, plusPtr, sizeOf)
data System = System
  { refRegion :: IORef BoundingBox2
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

thickness :: Float -> IO a -> IO a
thickness t m = do
  oldWidth <- liftIO $ get GL.lineWidth
  liftIO $ GL.lineWidth $= gf t
  res <- m
  liftIO $ GL.lineWidth $= oldWidth
  return res

installTexture :: Image PixelRGBA8 -> IO Texture
installTexture (Image w h v) = do
  [tex] <- GL.genObjectNames 1
  GL.textureBinding GL.Texture2D GL.$= Just tex
  let siz = GL.TextureSize2D (gsizei w) (gsizei h)
  V.unsafeWith v
    $ GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 siz 0
    . GL.PixelData GL.ABGR GL.UnsignedInt8888
  return (tex, fromIntegral w / 2, fromIntegral h / 2)

releaseTexture :: Texture -> IO ()
releaseTexture (tex, _, _) = GL.deleteObjectNames [tex]

beginFrame :: System -> IO ()
beginFrame sys = do
  -- Box (V2 wl wt) (V2 wr wb) <- fmap realToFrac <$> readIORef (refRegion sys)
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

endFrame :: System -> IO Bool
endFrame sys = do
  GLFW.swapBuffers $ theWindow sys
  GLFW.pollEvents
  GLFW.windowShouldClose (theWindow sys)

beginGLFW :: WindowMode -> BoundingBox2 -> IO System
beginGLFW mode bbox@(Box (V2 x0 y0) (V2 x1 y1)) = do
  Encoding.setForeignEncoding Encoding.utf8
  let title = "call"
      ww = floor $ x1 - x0
      wh = floor $ y1 - y0
  () <- unlessM GLFW.init (fail "Failed to initialize")

  mon <- case mode of
    FullScreen -> GLFW.getPrimaryMonitor
    _ -> return Nothing

  GLFW.windowHint $ GLFW.WindowHint'Resizable $ mode == Resizable
  win <- GLFW.createWindow ww wh title mon Nothing >>= maybe (fail "Failed to create a window") return
  GLFW.makeContextCurrent (Just win)
  prog <- initializeGL

  GLFW.swapInterval 1
  -- GL.clearColor $= GL.Color4 1 1 1 1
  
  rbox <- newIORef bbox

  GLFW.setFramebufferSizeCallback win $ Just $ \_ w h -> do
      modifyIORef rbox $ size zero .~ fmap fromIntegral (V2 w h)

  return $ System rbox win prog

initializeGL :: IO GL.Program
initializeGL = do
  let vertexAttribute = GL.AttribLocation 0
  let uvAttribute = GL.AttribLocation 1
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

  vertexShader <- GL.createShader GL.VertexShader
  fragmentShader <- GL.createShader GL.FragmentShader

  GL.shaderSourceBS vertexShader $= Text.encodeUtf8
    (Text.pack $ unlines
      [ "#version 330"
      , "uniform mat4 projection;"
      , "uniform mat4 matrices[13];"
      , "uniform int level;"
      , "out vec2 UV;"
      , "in vec3 in_Position;"
      , "in vec2 in_UV;"
      , "void main(void) {"
      , "  mat4 model = mat4(1.0);"
      , "  for(int i=0; i < level; i++)"
      , "  {"
      , "    model *= matrices[i];"
      , "  }"
      , "  gl_Position = projection * model * vec4(in_Position, 1.0);"
      , "  UV = in_UV;"
      , "}"
      ])
    
  GL.shaderSourceBS fragmentShader $= Text.encodeUtf8
    (Text.pack $ unlines
      [ "#version 330"
      , "in vec2 UV;"
      , "out vec4 fragColor;"
      , "uniform sampler2D tex;"
      , "uniform vec4 color = vec4(1,1,1,1);"
      , "void main(void){"
      , "  fragColor = texture( tex, UV ).rgba * color;"
      , "}"
      ])

  GL.compileShader vertexShader
  GL.compileShader fragmentShader
  shaderProg <- GL.createProgram
  GL.attachShader shaderProg vertexShader
  GL.attachShader shaderProg fragmentShader
  GL.attribLocation shaderProg "in_Position" $= vertexAttribute
  GL.attribLocation shaderProg "in_UV" $= uvAttribute
  GL.linkProgram shaderProg
  GL.currentProgram $= Just shaderProg

  linked <- GL.get (GL.linkStatus shaderProg)
  unless linked $ do
    GL.get (GL.programInfoLog shaderProg) >>= putStrLn
  
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.colorMask $= GL.Color4 GL.Enabled GL.Enabled GL.Enabled GL.Enabled
  GL.depthMask $= GL.Enabled
  GL.depthFunc $= Just GL.Lequal
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.textureFunction $= GL.Combine
  GL.clearColor $= GL.Color4 0.5 0.2 1 1

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
