{-# LANGUAGE TypeFamilies
  , FlexibleContexts
  , BangPatterns
  , DeriveFunctor
  , Rank2Types
  , FlexibleInstances
  , UndecidableInstances
  , ViewPatterns
  , ScopedTypeVariables
  , GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Scene
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Call.Sight (Affine(..)
    , Figure(..)
    , Picture(..)
    , bitmap
    , toward
    , Scene(..)
    , transformScene
    , vertices
    , Sight(..)
    , viewPicture
    , viewScene
    , VFX(..)
    , applyVFX
    , GL.PrimitiveMode(..)) where
import qualified Call.Data.Bitmap as B
import qualified Data.BoundingBox as X
import Data.Monoid
import Linear
import qualified Data.Vector.Storable as V
import Control.Lens
import qualified Graphics.Rendering.OpenGL.GL as GL
import Data.Color
import Call.Types

class Affine a where
  type Vec a :: *
  type Normal a :: *
  rotateOn :: Normal a -> a -> a
  scale :: Vec a -> a -> a
  translate :: Vec a -> a -> a

class Affine a => Figure a where
  primitive :: GL.PrimitiveMode -> [Vec a] -> a
  color :: RGBA -> a -> a
  line :: [Vec a] -> a
  polygon :: [Vec a] -> a
  polygonOutline :: [Vec a] -> a
  circle :: Normal a -> a
  circleOutline :: Normal a -> a

bitmap :: B.Bitmap -> Picture
bitmap bmp = Picture $ Scene
  $ \_ _ f _ _ -> f bmp GL.TriangleStrip
    (V.fromList [V3 (-w/2) (-h/2) 0 `positionUV` V2 0 0
        , V3 (w/2) (-h/2) 0 `positionUV` V2 1 0
        , V3 (-w/2) (h/2) 0 `positionUV` V2 0 1
        , V3 (w/2) (h/2) 0 `positionUV` V2 1 1]) where
  V2 w h = fmap fromIntegral $ B.size bmp

toward :: V3 Float -> Picture -> Scene
toward n@((^/norm n) -> V3 x y z) (Picture s) = transformScene
  (m33_to_m44 $ fromQuaternion $ axisAngle (V3 (-y) x 0) $ acos $ z / norm n)
  s

newtype Scene = Scene { unScene :: forall r.
  r
  -> (r -> r -> r)
  -> (B.Bitmap -> GL.PrimitiveMode -> V.Vector Vertex -> r)
  -> (VFX r -> r)
  -> (M44 Float -> r -> r)
  -> r
  }

data VFX r = SphericalAdd Bitmap r
  | SphericalMultiply Bitmap r
  | Diffuse RGBA r
  deriving Functor
  {-
  | Opacity r
  | Light RGBA Float (V3 Float) (V4 Float)
  | Specular RGB
  | Ambient RGB
  -}

instance Affine Scene where
  type Vec Scene = V3 Float
  type Normal Scene = V3 Float
  rotateOn v = transformScene $ m33_to_m44 $ fromQuaternion $ axisAngle v (norm v)
  scale (V3 x y z) = transformScene $ V4
    (V4 x 0 0 0)
    (V4 0 y 0 0)
    (V4 0 0 z 0)
    (V4 0 0 0 1)
  translate v = transformScene $ translation .~ v $ eye4

instance Figure Scene where
  primitive m vs = Scene $ \_ _ f _ _ -> f Blank m (V.fromList $ map positionOnly vs)
  color col (Scene s) = Scene $ \e a f b t -> b (Diffuse col (s e a f b t))
  line = primitive GL.LineStrip
  polygon = primitive GL.Polygon
  polygonOutline = primitive GL.LineLoop
  circle v = toward v $ circle $ norm v
  circleOutline v = toward v $ circleOutline $ norm v

unit_circle :: Int -> [V2 Float]
unit_circle n = map angle [0,2*pi/fromIntegral n..2*pi]

instance Monoid Scene where
  mempty = Scene $ \e _ _ _ _ -> e
  mappend (Scene x) (Scene y) = Scene $ \e a f b t -> a (x e a f b t) (y e a f b t)

v2ToV3 :: Num a => V2 a -> V3 a
v2ToV3 (V2 x y) = V3 x y 0

vertices :: B.Bitmap -> GL.PrimitiveMode -> V.Vector Vertex -> Scene
vertices b m v = Scene $ \_ _ f _ _ -> f b m v

transformScene :: M44 Float -> Scene -> Scene
transformScene m (Scene pic) = Scene $ \e a f b t -> t m (pic e a f b t)

applyVFX :: VFX Scene -> Scene
applyVFX vf = Scene $ \e a f b t -> b $ fmap (\(Scene s) -> s e a f b t) vf

newtype Picture = Picture { unPicture :: Scene } deriving Monoid

instance Affine Picture where
  type Vec Picture = V2 Float
  type Normal Picture = Float
  rotateOn t (Picture s) = Picture (transformScene m s) where
    m = V4 (V4 (cos t) (-sin t) 0 0) (V4 (sin t) (cos t) 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
  translate (V2 x y) (Picture s) = Picture $ transformScene (translation .~ V3 x y 0 $ eye4) s
  scale (V2 x y) (Picture s) = Picture (transformScene m s) where
    m = V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 1 0) (V4 0 0 0 1)

instance Figure Picture where
  primitive m v = Picture $ primitive m $ map v2ToV3 v
  color col (Picture s) = Picture (color col s)
  line = primitive GL.LineStrip
  polygon = primitive GL.Polygon
  polygonOutline = primitive GL.LineLoop
  circle r = polygon $ map (^*r) $ unit_circle 33
  circleOutline r = polygonOutline $ map (^*r) $ unit_circle 33

newtype Sight = Sight { unSight
  :: forall r. 
  X.Box V2 Float
  -> r
  -> (r -> r -> r)
  -> (X.Box V2 Float -> M44 Float -> Bool -> Scene -> r)
  -> r
  }

instance Monoid Sight where
  mempty = Sight $ \_ e _ _ -> e
  mappend (Sight x) (Sight y) = Sight $ \b e a f -> a (x b e a f) (y b e a f)

viewPicture :: Picture -> Sight
viewPicture (Picture s) = Sight $ \box@(X.Box (V2 x0 y0) (V2 x1 y1)) _ _ f -> f box (ortho x0 x1 y1 y0 (-1) 1) False s

viewScene :: Float -> Float -> Float -> Scene -> Sight
viewScene fov near far s = Sight $ \box _ _ f -> f box (perspective fov (let V2 w h = box ^. X.size 0 in w/h) near far) True s
