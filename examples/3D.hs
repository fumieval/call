{-# LANGUAGE TemplateHaskell, LambdaCase #-}
import Call
import Call.Data.Bitmap (Bitmap(..))
import Control.Lens
import qualified Data.Vector.Storable as V
import System.IO.Unsafe
import Data.Monoid
import qualified Data.Map as Map
import Control.Monad.Trans
import Data.BoundingBox (Box(..))
import Debug.Trace
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Foldable as F
import qualified Graphics.Rendering.OpenGL as GL
import Data.Distributive (distribute)

bmp_stonebrick = unsafePerformIO $ readBitmap "examples/stonebrick.png"
bmp_dirt = unsafePerformIO $ readBitmap "examples/dirt.png"
bmp_crosshair = unsafePerformIO $ readBitmap "examples/crosshair.png"

data BlockType = StoneBrick | Dirt

data Block = Block
  { _blockType :: BlockType }
makeLenses ''Block

data World = World
  { _blocks :: Map.Map (V3 Int) Block
  , _playerPos :: Vec Scene
  , _playerPos' :: Vec Scene
  , _playerVelocity :: Vec Scene
  , _playerAngle :: V2 Float
  , _focused :: Maybe (V3 Int, V3 Int)
  , _time :: Float
  }
makeLenses ''World

block :: Bitmap -> Scene
block bmp = v0 <> v1 <> v2 where
  mk = vertices bmp TriangleStrip . V.fromList . map offset
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
  offset (Vertex p uv) = Vertex (p - pure 0.5) uv

theFloor = [(V3 c 0 r, Block Dirt) | c <- [-16..16], r <- [-16..16]]

renderBlock :: Block -> Scene
renderBlock (Block Dirt) = block bmp_dirt
renderBlock (Block StoneBrick) = block bmp_stonebrick

renderBlocks :: Time -> Map.Map (V3 Int) Block -> Scene
renderBlocks t m = mconcat [translate (fmap fromIntegral p) $ renderBlock b
  | (p, b) <- Map.toList m]

facing :: V3 Int -> V3 Float -> V3 Float -> [(Float, (V3 Int, V3 Int))]
facing b p dir = [(norm (p - m), (b, fmap floor n))
  | n <- [V3 0 0 1, V3 0 1 0, V3 1 0 0, V3 0 0 (-1), V3 0 (-1) 0, V3 (-1) 0 0]
  , let nd = dot dir n
  , nd < 0
  , let m = p - dir ^* (abs (dot (b' + n ^* 0.5 - p) n) / nd)
  , F.all ((<=0.50001) . abs) (m - b')
  ]
  where
    b' = fmap fromIntegral b

applyPlay t v
  | norm v < t = zero
  | otherwise = v

updatePlayer dt = do
  (p:_) <- lift getGamepads
  (mx : mz : px : py : _) <- lift $ gamepadAxes p
  playerAngle += applyPlay 0.005 (V2 px py ^/ 8)

  pos <- use playerPos
  V2 dir elev <- use playerAngle
  let V2 lx lz = distribute $ V3 (angle dir) 0 (-perp (angle dir))
  playerPos' += applyPlay 0.005 ((lz ^* mz + lx ^* mx) ^/ 8)
  
  vel <- use playerVelocity
  playerPos' += vel
  playerVelocity += V3 0 (-0.07) 0

  bs <- use blocks

  pos' <- use playerPos'
  when (fmap round (pos' - V3 0 1 0) `Map.member` bs) $ do
    playerPos . _y .= view _y pos
    playerVelocity . _y .= 0

  playerPos <~ use playerPos'

  let aim = spherical dir elev
  case sortBy (compare `on` fst) [v | i <- Map.keys bs
    , fmap fromIntegral i `qd` pos < 8*8
    , v@(_, (p, n)) <- facing i pos aim
    , Map.notMember (p + n) bs] of
    ((_, v):_) -> focused ?= v
    [] -> focused .= Nothing

spherical dir elev = V3 (sin dir * cos elev) (-sin elev) (-cos dir * cos elev)

main = runSystem FullScreen (Box (V2 0 0) (V2 1600 900)) $ do
  world <- new $ variable $ World
    (Map.fromList theFloor)
    (V3 0 2 0)
    (V3 0 2 0)
    (V3 0 0 0)
    (V2 0 0)
    Nothing
    0
  newJoypad $ liftO $ accept $ ((world .&) .) $ \case
    PadButton _ (Down 7) -> use focused >>= \case
      Just (p, _) -> blocks . at p .= Nothing
      Nothing -> return ()
    PadButton _ (Down 5) -> use focused >>= \case
      Just (p, n) -> blocks . at (p + n) ?= Block StoneBrick
      Nothing -> return ()
    PadButton _ (Down 6) -> playerVelocity += V3 0 0.5 0
    PadButton _ (Down i) -> liftIO $ print i
    _ -> return ()

  newGraphic $ liftO $ accept $ \dt -> world .& do
    time += dt
    t <- use time

    updatePlayer dt
    
    mark <- uses focused $ \case
      Just (p, n) -> let q = fmap fromIntegral p in
        vertices Blank LineStrip $ V.fromList [q `Vertex` 0, (q + fmap fromIntegral n ^* 2) `Vertex` 0]
      Nothing -> mempty
    liftIO $ GL.lineWidth GL.$= 16
    pos <- use playerPos

    V2 dir elev <- use playerAngle
    bs <- use blocks
    return $ mconcat
      [viewScene (pi / 4) 1 1000
        $ rotateOn (V3 elev 0 0) $ rotateOn (V3 0 dir 0) $ translate (-pos) $ mconcat
          [renderBlocks t bs
          , mark
          , translate (V3 0 2 0) $ toward (normalize $ V3 0 2 0 - pos) $ scale (1/48) $ bitmap bmp_crosshair
          ]
      ]
  stand