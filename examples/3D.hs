import Call
import Control.Lens
import Control.Monad.Objective
import qualified Data.Vector.Storable as V
import Foreign.C (CFloat)
import Call.Data.Bitmap (Bitmap(..))
import System.IO.Unsafe
bmp = unsafePerformIO $ readBitmap "examples/stonebrick.png"

block :: Picture
block = vertices bmp $ V.fromList $ []
      V3 0 0 0 
    , V3 0 0 1
    , V3 0 1 0
    , V3 0 1 1
    , V3 1 0 0
    , V3 1 0 1
    , V3 1 1 0
    , V3 1 1 1
    ]

triangleTransformation :: (Epsilon a, Floating a) => a -> M44 a
triangleTransformation =
  liftA2 (!*!) triangleTranslation triangleRotation

--------------------------------------------------------------------------------
triangleRotation :: (Epsilon a, Floating a) => a -> M44 a
triangleRotation t = m33_to_m44 $ fromQuaternion $ axisAngle (V3 0 1 0) (t * 2)

triangleTranslation :: Floating a => a -> M44 a
triangleTranslation t =
  eye4 & translation .~ V3 (sin t * 2) 0 (-5)

main = runSystemDefault $ do
    obj <- new $ animate $ \t -> projectPicture (perspective 120 (4/3) 1 1000)
        $ Call.transform (triangleTransformation $ realToFrac t) block
    linkGraphic obj
    stand