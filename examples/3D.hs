{-# LANGUAGE TemplateHaskell #-}
import Call
import Call.Data.Bitmap (Bitmap)
import Control.Lens
import qualified Data.Vector.Storable as V
import System.IO.Unsafe
import Data.Monoid
import qualified Data.Map as Map
stonebrick = unsafePerformIO $ readBitmap "examples/stonebrick.png"

data BlockType = StoneBrick

data Block = Block
  { _blockType :: BlockType }
makeLenses ''Block

data World = World
  { _blocks :: Map.Map (V3 Int) Block
  , _playerPos :: Vec Scene
  , _playerDir :: Float
  }
makeLenses ''World

block :: Bitmap -> Scene
block bmp = v0 <> v1 <> v2 where
  mk = vertices bmp TriangleStrip . V.fromList
  v0 = mk [
      V3 0 0 0 `Vertex` V2 0 0
    , V3 0 0 1 `Vertex` V2 0 1
    , V3 0 1 0 `Vertex` V2 1 0
    , V3 0 1 1 `Vertex` V2 1 1
    , V3 1 1 0 `Vertex` V2 0 0
    , V3 1 1 1 `Vertex` V2 0 1
    ]
  v1 = mk [
      V3 0 0 0 `Vertex` V2 0 0
    , V3 0 1 0 `Vertex` V2 0 1
    , V3 1 0 0 `Vertex` V2 1 0
    , V3 1 1 0 `Vertex` V2 1 1
    , V3 1 0 1 `Vertex` V2 0 0
    , V3 1 1 1 `Vertex` V2 0 1
    ]
  v2 = mk [
      V3 0 0 0 `Vertex` V2 0 0
    , V3 1 0 0 `Vertex` V2 0 1
    , V3 0 0 1 `Vertex` V2 1 0
    , V3 1 0 1 `Vertex` V2 1 1
    , V3 0 1 1 `Vertex` V2 0 0
    , V3 1 1 1 `Vertex` V2 0 1
    ]

theFloor = [(V3 c 0 r, Block StoneBrick) | c <- [-16..16], r <- [-16..16]]

renderBlocks :: Map.Map (V3 Int) Block -> Scene
renderBlocks m = mconcat [translate (fmap fromIntegral p) $ block stonebrick | (p, b) <- Map.toList m]

main = runSystemDefault $ do
  world <- new $ variable $ World (Map.fromList theFloor) (V3 0 0 0) 0
  newGraphic $ liftO $ accept $ \dt -> do
    whenM (keyPress KeySpace) $ world .& playerPos += V3 0 dt 0
    whenM (keyPress KeyLeftShift) $ world .& playerPos += V3 0 (-dt) 0
    dir <- world .& use playerDir
    whenM (keyPress KeyW) $ world .& playerPos += V3 (sin dir) 0 (-cos dir) ^* (2 * dt)
    whenM (keyPress KeyS) $ world .& playerPos -= V3 (sin dir) 0 (-cos dir) ^* (2 * dt)
    whenM (keyPress KeyA) $ world .& playerDir .= dir - dt
    whenM (keyPress KeyD) $ world .& playerDir .= dir + dt
    pos <- world .& use playerPos
    bs <- world .& uses blocks renderBlocks
    return $ viewSight (pi / 4) 1 1000 $ rotateOn (V3 0 dir 0) $ translate (-pos) $ bs
  stand