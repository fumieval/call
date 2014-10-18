{-# LANGUAGE BangPatterns, DeriveFunctor, Rank2Types, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
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

infixr 5 `translate`
infixr 5 `rotateR`
infixr 5 `rotateD`
infixr 5 `scale`
infixr 5 `color`
infixr 5 `thickness`
infixr 5 `blendMode`

class Affine a where
    type Vec a :: *
    type Normal a :: * 
    rotateOn :: Normal -> a -> a
    scale :: Vec -> a -> a
    translate :: Vec -> a -> a

class Affine a => Picture a where
    line :: [Vec] -> a
    polygon :: [Vec] -> a
    polygonOutline :: [Vec] -> a
    circle :: Normal -> a
    circleOutline :: Normal -> a
    bitmapToward :: Normal -> Bitmap -> a
    polygonWith :: Bitmap -> [(V2 Double, Vec)] -> a

bitmap = bitmapToward 1

class Decorate a where
    thickness :: Float -> a -> a
    color :: Color -> a -> a
    blendMode :: BlendMode -> a -> a

class Affine a => Perspective a where
    viewFromTo :: Double -> Vec -> Normal -> a -> a

class Affine a => Lighting a where
    light :: Normal -> Double -> a

opacity :: Decorate a => Float -> a -> a
opacity a = color (Color 1 1 1 a)

instance Monoid a => Monoid (Picture a) where
    mempty = return mempty
    mappend p q = mappend <$> p <*> q

instance Functor Picture where
    fmap f (Picture m) = Picture (fmap f m)

instance Applicative Picture where
    pure a = Picture (pure a)
    Picture a <*> Picture b = Picture (a <*> b)

instance Monad Picture where
    return a = Picture (return a)
    Picture m >>= k = Picture (m >>= runPicture . k)

mapResponse :: (c -> b) -> Request a b r -> Request a c r
mapResponse f (Request a cont) = Request a (cont . f)
