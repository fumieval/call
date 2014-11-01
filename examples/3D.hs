import Call
import Control.Lens
import Control.Monad.Objective
import qualified Data.Vector.Storable as V
import Foreign.C (CFloat)
import Call.Data.Bitmap (Bitmap(..))
import System.IO.Unsafe
import Data.Monoid
bmp = unsafePerformIO $ readBitmap "examples/stonebrick.png"

block :: Picture
block = v0 <> v1 <> v2 where
  v0 = vertices bmp $ V.fromList [
      V3 0 0 0 `Vertex` V2 0 0
    , V3 0 0 1 `Vertex` V2 0 1
    , V3 0 1 0 `Vertex` V2 1 0
    , V3 0 1 1 `Vertex` V2 1 1
    , V3 1 1 0 `Vertex` V2 1 0
    , V3 1 1 1 `Vertex` V2 0 1
    ]
  v1 = vertices bmp $ V.fromList [
      V3 0 0 0 `Vertex` V2 0 0
    , V3 0 1 0 `Vertex` V2 0 1
    , V3 1 0 0 `Vertex` V2 1 0
    , V3 1 1 0 `Vertex` V2 1 1
    , V3 1 0 1 `Vertex` V2 0 0
    , V3 1 1 1 `Vertex` V2 0 1
    ]
  v2 = vertices bmp $ V.fromList [
      V3 0 0 0 `Vertex` V2 0 0
    , V3 1 0 0 `Vertex` V2 0 1
    , V3 0 0 1 `Vertex` V2 1 0
    , V3 1 0 1 `Vertex` V2 1 1
    , V3 0 1 1 `Vertex` V2 0 0
    , V3 1 1 1 `Vertex` V2 0 1
    ]

transformation :: (Epsilon a, Floating a) => a -> M44 a
transformation = liftA2 (!*!) b a where
  a t = m33_to_m44 $ fromQuaternion $ axisAngle (V3 0 1 0) (t * 2)
  b t = eye4 & translation .~ V3 (sin t * 2) (cos (t / 4) * 2) (-5)

main = runSystemDefault $ do
    obj <- new $ animate $ \t -> projectPicture (perspective 120 (4/3) 1 1000)
        $ transformPicture (transformation t) block
    linkGraphic obj
    stand