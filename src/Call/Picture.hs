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
import Call.Types
import Call.Data.Bitmap
import Data.Color
import Control.Applicative
import Data.Monoid
import Control.Object
import Data.OpenUnion1.Clean
import Linear
import Linear.Matrix
import Foreign.C (CFloat)
import Foreign.Storable
import Foreign.Ptr
import qualified Data.Vector.Storable as V

class Affine a where
    type Vec a :: *
    type Normal a :: * 
    rotateOn :: Normal a -> a -> a
    scale :: Vec a -> a -> a
    translate :: Vec a -> a -> a

class Affine a => Figure a where
    line :: [Vec a] -> a
    polygon :: [Vec a] -> a
    polygonOutline :: [Vec a] -> a
    circle :: Normal a -> a
    circleOutline :: Normal a -> a
    bitmapToward :: Normal a -> Bitmap -> a

bitmap :: (Figure a, Num (Normal a)) => Bitmap -> a
bitmap = bitmapToward 1

newtype Picture = Picture { unPicture :: forall r.
    r
    -> (r -> r -> r)
    -> (Bitmap -> V.Vector Vertex -> r)
    -> (M44 CFloat -> r -> r)
    -> (M44 CFloat -> r -> r)
    -> r
    }

mapResponse :: (c -> b) -> Request a b r -> Request a c r
mapResponse f (Request a cont) = Request a (cont . f)

data Vertex = Vertex { vPos :: {-# UNPACK #-} !(V3 CFloat), vUV :: {-# UNPACK #-} !(V2 CFloat)}

instance Storable Vertex where
    sizeOf _ = sizeOf (undefined :: V3 CFloat) + sizeOf (undefined :: V2 CFloat)
    alignment _ = 0
    peek ptr = Vertex <$> peek (castPtr ptr) <*> peek (castPtr $ ptr `plusPtr` sizeOf (vPos undefined))
    poke ptr (Vertex v t) = do
        poke (castPtr ptr) v
        poke (castPtr ptr  `plusPtr` sizeOf (vPos undefined)) v

vertices :: Bitmap -> V.Vector Vertex -> Picture
vertices b v = Picture $ \_ _ f _ _ -> f b v

transform :: M44 CFloat -> Picture -> Picture
transform m (Picture pic) = Picture $ \e a f p t -> t m (pic e a f p t)

projectPicture :: M44 CFloat -> Picture -> Picture
projectPicture m (Picture pic) = Picture $ \e a f p t -> p m (pic e a f p t)