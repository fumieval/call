{-# LANGUAGE TemplateHaskell, LambdaCase, Rank2Types #-}
import Call
import Control.Lens
import qualified Data.Vector.Storable as V
import System.IO.Unsafe
import Data.Monoid
import qualified Data.Map.Strict as Map
import Control.Monad.Trans
import Data.BoundingBox (Box(..))
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Foldable as F
import qualified Data.Set as Set
import Data.Distributive (distribute)

-- Textures: http://forum.minecraftuser.jp/viewtopic.php?t=14102
bmp_stonebrick = unsafePerformIO $ readBitmap "examples/stonebrick.png"
bmp_dirt = unsafePerformIO $ readBitmap "examples/dirt.png"
bmp_crosshair = unsafePerformIO $ readBitmap "examples/crosshair.png"
bmp_skybox = unsafePerformIO $ readBitmap "examples/skybox.png"
bmp_logo = unsafePerformIO $ readBitmap "examples/logo.png"

data Surface = STop | SBottom | SLeft | SRight | SFront | SRear deriving (Show, Eq, Ord)

fromSurface STop = V3 0 1 0
fromSurface SBottom = V3 0 (-1) 0
fromSurface SLeft = V3 (-1) 0 0
fromSurface SRight = V3 1 0 0
fromSurface SFront = V3 0 0 1
fromSurface SRear = V3 0 0 (-1)

allSurfaces = [STop, SBottom, SLeft, SRight, SFront, SRear]

skybox = scale (V3 128 128 128)
   $ surfaceBitmap bmp_skybox [V2 (1/3) 1, V2 (2/3) 1, V2 (1/3) 0.5, V2 (2/3) 0.5] SRear
  <> surfaceBitmap bmp_skybox [V2 (1/3) 1, V2 0 1, V2 (1/3) 0.5, V2 0 0.5] SLeft
  <> surfaceBitmap bmp_skybox [V2 (2/3) 1, V2 1 1, V2 (2/3) 0.5, V2 1 0.5] SRight
  <> surfaceBitmap bmp_skybox [V2 (1/3) 0.5, V2 (2/3) 0.5, V2 (1/3) 0, V2 (2/3) 0] STop
  <> surfaceBitmap bmp_skybox [V2 1 0.5, V2 (2/3) 0.5, V2 1 0, V2 (2/3) 0] SFront
  <> surfaceBitmap bmp_skybox [V2 0 0, V2 (1/3) 0, V2 0 0.5, V2 (1/3) 0.5] SBottom

surfaceBitmap :: Bitmap -> [V2 Float] -> Surface -> Scene
surfaceBitmap bmp uvs = \case
  SRear -> mk [V3 0 0 0, V3 1 0 0, V3 0 1 0, V3 1 1 0]
  SLeft -> mk [V3 0 0 0, V3 0 0 1, V3 0 1 0, V3 0 1 1] 
  SRight -> mk [V3 1 0 0, V3 1 0 1, V3 1 1 0, V3 1 1 1]
  STop -> mk [V3 0 1 0, V3 1 1 0, V3 0 1 1, V3 1 1 1]
  SFront -> mk [V3 0 0 1, V3 1 0 1, V3 0 1 1, V3 1 1 1]
  SBottom -> mk [V3 0 0 0, V3 1 0 0, V3 0 0 1, V3 1 0 1]
  where
    mk vs = vertices bmp TriangleStrip $ V.fromList $ map offset $ zipWith (flip Vertex) uvs vs
    {-# INLINE mk #-}
    offset (Vertex p uv) = Vertex (p - pure 0.5) uv
    {-# INLINE offset #-}
{-# INLINE surfaceBitmap #-}

neumann :: [V3 Int]
neumann = [V3 0 0 1, V3 0 1 0, V3 1 0 0, V3 0 0 (-1), V3 0 (-1) 0, V3 (-1) 0 0]

class Substantial a where
  isOpaque :: a -> Bool

instance Substantial Block where
  isOpaque _ = True

data VoxelWorld a = VoxelWorld !(Map.Map (V3 Int) a) !(Map.Map (V3 Int) (Set.Set Surface))

prepare :: V3 Int -> a -> [V3 Int] -> Map.Map (V3 Int) a -> Map.Map (V3 Int) a
prepare v a = flip $ foldr (\d -> at (v + d) %~ maybe (Just a) Just)

voxelAt :: Substantial a => V3 Int -> Lens' (VoxelWorld a) (Maybe a)
voxelAt v f (VoxelWorld m s) = f (Map.lookup v m) <&> \case
  Nothing -> VoxelWorld (Map.delete v m) $ s
    & Map.delete v
    & flip (foldr (\d -> ix (v - fromSurface d) . contains d .~ True)) allSurfaces
  Just a -> VoxelWorld (Map.insert v a m) $ s
    & prepare v Set.empty neumann
    & at v ?~ Set.fromList allSurfaces Set.\\ Set.fromList [s | s <- allSurfaces, b <- m ^.. ix (v + fromSurface s), isOpaque b]
    & if isOpaque a then flip (foldr (\d -> ix (v - fromSurface d) . contains d .~ False)) allSurfaces else id

data BlockType = StoneBrick | Dirt

data Block = Block
  { _blockType :: BlockType }
makeLenses ''Block

data World = World
  { _blocks :: VoxelWorld Block
  , _playerPos :: Vec Scene
  , _playerPos' :: Vec Scene
  , _playerVelocity :: Vec Scene
  , _playerAngle :: V2 Float
  , _focused :: Maybe (V3 Int, V3 Int)
  , _time :: Float
  }
makeLenses ''World

renderBlock :: Block -> [Surface] -> Scene
renderBlock (Block b) ss = case b of
  Dirt -> r bmp_dirt
  StoneBrick -> r bmp_stonebrick
  where
    r bmp = mconcat $ map (surfaceBitmap bmp [V2 0 0, V2 1 0, V2 0 1, V2 1 1]) ss

renderBlocks :: Time -> VoxelWorld Block -> Scene
renderBlocks t (VoxelWorld m s) = mconcat [translate (fmap fromIntegral p)
  $ renderBlock b (s ^.. ix p . folded)
  | (p, b) <- Map.toList m]

facing :: V3 Int -> V3 Float -> V3 Float -> V3 Float -> [(Float, (V3 Int, V3 Int))]
facing b p dir n = [(norm (p - m), (b, fmap floor n))
  | let nd = dot dir n
  , nd < 0
  , let m = p - dir ^* (abs (dot (b' + n ^* 0.5 - p) n) / nd)
  , F.all ((<=0.50001) . abs) (m - b')
  ]
  where
    b' = fmap fromIntegral b

applyPlay :: Metric f => Float -> f Float -> f Float
applyPlay t v
  | q < t*t = zero
  | otherwise = v ^* q ** 0.25
  where
    q = quadrance v

updatePlayer dt = do
  (p:_) <- lift getGamepads
  (mx : mz : px : py : _) <- lift $ gamepadAxes p
  playerAngle += applyPlay 0.001 (V2 px py ^* dt ^* 4)

  pos <- use playerPos
  V2 dir elev <- use playerAngle
  let V2 lx lz = distribute $ V3 (angle dir) 0 (-perp (angle dir))
  playerPos' += applyPlay 0.001 ((lz ^* mz + lx ^* mx) ^* dt ^* 6)
  
  vel <- use playerVelocity
  playerPos' += vel
  playerVelocity += V3 0 (-0.07) 0

  VoxelWorld bs ss <- use blocks

  pos' <- use playerPos'
  when (fmap round (pos' - V3 0 1 0) `Map.member` bs) $ do
    playerPos . _y .= view _y pos
    playerVelocity . _y .= 0

  playerPos <~ use playerPos'

  let aim = spherical dir elev
  case sortBy (compare `on` fst) [v | (i, s) <- ss ^@.. itraversed <. folded
    , fmap fromIntegral i `qd` pos < 8*8
    , v@(_, (p, n)) <- facing i pos aim (fromSurface s)
    , Map.notMember (p + n) bs] of
    ((_, v):_) -> focused ?= v
    [] -> focused .= Nothing

spherical :: RealFloat a => a -> a -> V3 a
spherical dir elev = V3 (sin dir * cos elev) (-sin elev) (-cos dir * cos elev)

toward' :: V3 Float -> Picture -> Scene
toward' n@(V3 x y z) (Picture s) = transformScene
  ((!!*norm n) $ m33_to_m44 $ fromQuaternion
    $ axisAngle (V3 (-y) x 0) (-asin (sqrt (x*x+y*y))))
  s

main = runSystem Windowed (Box (V2 0 0) (V2 1600 900)) $ do
  setFPS 30
  world <- new $ variable $ World
    (foldr (\(i, v) -> voxelAt i ?~ v) (VoxelWorld Map.empty Map.empty) [(V3 c 0 r, Block Dirt) | c <- [-16..16], r <- [-16..16]])
    (V3 0 2 0)
    (V3 0 2 0)
    (V3 0 0 0)
    (V2 0 0)
    Nothing
    0
  newJoypad $ liftO $ accept $ ((world .&) .) $ \case
    PadButton _ (Down 7) -> use focused >>= \case
      Just (p, _) -> blocks . voxelAt p .= Nothing
      Nothing -> return ()
    PadButton _ (Down 5) -> use focused >>= \case
      Just (p, n) -> blocks . voxelAt (p + n) ?= Block StoneBrick
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
    pos <- use playerPos

    V2 dir elev <- use playerAngle
    bs <- use blocks
    return $ mconcat [viewScene (pi / 4) 1 200
      $ rotateOn (V3 elev 0 0) $ rotateOn (V3 0 dir 0) $ translate (-pos) $ mconcat
        [ translate pos skybox
        , renderBlocks t bs
        , mark
        , translate (V3 0 2 0) $ toward' (normalize $ V3 0 2 0 - pos) $ scale (1/32) $ bitmap bmp_logo
        ], viewPicture $ translate (V2 800 450) $ bitmap bmp_crosshair]
  stand