{-# LANGUAGE TypeFamilies, FlexibleContexts, BangPatterns, DeriveFunctor, Rank2Types, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Picture
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Call.Picture where
import Call.Data.Bitmap
import Control.Applicative
import Data.Monoid
import Linear
import Foreign.Storable
import Foreign.Ptr
import qualified Data.Vector.Storable as V
import Control.Lens
import qualified Graphics.Rendering.OpenGL.GL as GL

class Affine a where
    type Vec a :: *
    type Normal a :: * 
    rotateOn :: Normal a -> a -> a
    scale :: Vec a -> a -> a
    translate :: Vec a -> a -> a
{-
class Affine a => Figure a where
    line :: [Vec a] -> a
    polygon :: [Vec a] -> a
    polygonOutline :: [Vec a] -> a
    circle :: Normal a -> a
    circleOutline :: Normal a -> a
    bitmapToward :: Normal a -> Bitmap -> a

bitmap :: (Figure a, Num (Normal a)) => Bitmap -> a
bitmap = bitmapToward 1
-}

instance Affine Picture where
    type Vec Picture = V3 Float
    type Normal Picture = V3 Float
    rotateOn v = transformPicture $ m33_to_m44 $ fromQuaternion $ axisAngle v (norm v)
    scale (V3 x y z) = transformPicture $ V4
        (V4 x 0 0 0)
        (V4 0 y 0 0)
        (V4 0 0 z 0)
        (V4 0 0 0 1)
    translate v (Picture pic) = Picture $ \e a f p t -> t (translation +~ v) (pic e a f p t)

newtype Picture = Picture { unPicture :: forall r.
    r
    -> (r -> r -> r)
    -> (Bitmap -> GL.PrimitiveMode -> V.Vector Vertex -> r)
    -> (M44 Float -> r -> r)
    -> ((M44 Float -> M44 Float) -> r -> r)
    -> r
    }

instance Monoid Picture where
    mempty = Picture $ \e _ _ _ _ -> e
    mappend (Picture x) (Picture y) = Picture $ \e a f p t -> a (x e a f p t) (y e a f p t)

data Vertex = Vertex { vPos :: {-# UNPACK #-} !(V3 Float), vUV :: {-# UNPACK #-} !(V2 Float)}

instance Storable Vertex where
    sizeOf _ = sizeOf (undefined :: V3 Float) + sizeOf (undefined :: V2 Float)
    alignment _ = 0
    peek ptr = Vertex <$> peek (castPtr ptr) <*> peek (castPtr $ ptr `plusPtr` sizeOf (vPos undefined))
    poke ptr (Vertex v t) = do
        poke (castPtr ptr) v
        poke (castPtr ptr `plusPtr` sizeOf (0 :: V3 Float)) t

vertices :: Bitmap -> GL.PrimitiveMode -> V.Vector Vertex -> Picture
vertices b m v = Picture $ \_ _ f _ _ -> f b m v

transformPicture :: M44 Float -> Picture -> Picture
transformPicture m (Picture pic) = Picture $ \e a f p t -> t (!*! m) (pic e a f p t)

projectPicture :: M44 Float -> Picture -> Picture
projectPicture m (Picture pic) = Picture $ \e a f p t -> p m (pic e a f p t)