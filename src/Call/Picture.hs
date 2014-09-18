{-# LANGUAGE BangPatterns, DeriveFunctor, Rank2Types #-}
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

infixr 5 `translate`
infixr 5 `rotateR`
infixr 5 `rotateD`
infixr 5 `scale`
infixr 5 `color`
infixr 5 `thickness`
infixr 5 `blendMode`

class Functor p => Affine p where
    -- | (radians)
    rotateR :: Double -> p a -> p a
    -- | (degrees)
    rotateD :: Double -> p a -> p a
    scale :: Vec2 -> p a -> p a
    translate :: Vec2 -> p a -> p a

    rotateR = rotateD . (* 180) . (/ pi)
    rotateD = rotateR . (/ 180) . (* pi)

-- | The class of types that can be regarded as a kind of picture.
class Affine p => Picture2D p where
    -- | Construct a 'Picture2D' from a 'Bitmap'.
    bitmap :: Bitmap -> p ()
    -- | Same as 'bitmap', but it does not create a cache.
    bitmapOnce :: Bitmap -> p ()
    line :: [Vec2] -> p ()
    polygon :: [Vec2] -> p ()
    polygonOutline :: [Vec2] -> p ()
    circle :: Double -> p ()
    circleOutline :: Double -> p ()
    thickness :: Float -> p a -> p a
    color :: Color -> p a -> p a
    blendMode :: BlendMode -> p a -> p a

newtype Picture a = Picture { runPicture :: forall m. (Applicative m, Monad m, Picture2D m) => m a }

instance Functor Picture where
    fmap f (Picture m) = Picture (fmap f m)

instance Applicative Picture where
    pure a = Picture (pure a)
    Picture a <*> Picture b = Picture (a <*> b)

instance Monad Picture where
    return a = Picture (return a)
    Picture m >>= k = Picture (m >>= runPicture . k)

instance Affine Picture where
    rotateR t (Picture m) = Picture (rotateR t m)
    rotateD t (Picture m) = Picture (rotateD t m)
    scale v (Picture m) = Picture (scale v m)
    translate v (Picture m) = Picture (translate v m)

instance Picture2D Picture where
    bitmap b = Picture (bitmap b)
    bitmapOnce b = Picture (bitmapOnce b)
    line vs = Picture (line vs)
    polygon vs = Picture (polygon vs)
    polygonOutline vs = Picture (polygonOutline vs)
    circle r = Picture (circle r)
    circleOutline r = Picture (circleOutline r)
    thickness t (Picture m) = Picture (thickness t m)
    color c (Picture m) = Picture (color c m)
    blendMode b (Picture m) = Picture (blendMode b m)
