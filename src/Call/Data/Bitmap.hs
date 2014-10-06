{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.Data.Bitmap
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Call.Data.Bitmap (
  Bitmap(Blank, Bitmap)
  ,image
  ,offset
  ,hash
  ,liftImage
  ,liftImage'
  ,size
  ,clip
  ,bbox
  ,readFile
  ,writeFile
  ) where
import Prelude hiding (readFile, writeFile)
import qualified Codec.Picture as C
import qualified Codec.Picture.Types as C
import qualified Codec.Picture.RGBA8 as C
import Control.Monad.IO.Class
import qualified Data.BoundingBox as B
import Linear
import System.Random
import Control.Applicative
import qualified Data.Hashable as H
import qualified Data.Vector.Storable as V
import Data.Monoid
import Control.Lens
import Control.Monad.ST

data Bitmap = Blank | Bitmap { _image :: C.Image C.PixelRGBA8, _offset :: V2 Int, _hash :: Int }

makeLenses ''Bitmap

-- | `mappend` stitches the right operand to the left
instance Monoid Bitmap where
  mempty = Blank
  mappend base@(Bitmap b (V2 x0 y0) h0) lay@(Bitmap l (V2 x1 y1) h1) = runST $ do
    let box = B.union (bbox base) (bbox lay)
    let V2 w h = box ^. B.size 0
    img <- C.createMutableImage w h (C.PixelRGBA8 0 0 0 0) >>= C.unsafeFreezeImage
    let ox = max 0 (x0 - x1)
    let oy = max 0 (y0 - y1)
    return $ Bitmap
      (C.patchImage (C.patchImage img (ox, oy) b) (max 0 (x1 - x0), max 0 (y1 - y0)) l)
      (V2 (x0 + ox) (y0 + oy))
      (H.hash (h0, h1))
  mappend Blank b = b
  mappend b Blank = b

clip :: Bitmap -> B.Box V2 Int -> Bitmap
clip (Bitmap b (V2 ox oy) k) (B.Box (V2 x0 y0) (V2 x1 y1)) = Bitmap
  (C.trimImage b (x1 - x0, y1 - y0) (x0 - ox, y0 - oy))
  (V2 ox oy)
  (H.hash (x0, y0, x1, y1, k))

clip Blank _ = Blank

bbox :: Bitmap -> B.Box V2 Int
bbox (Bitmap (C.Image w h _) (V2 x y) _) = B.Box (V2 x y) (V2 (x+w) (y+h))
bbox Blank = B.Box zero zero

size :: Bitmap -> V2 Int
size (Bitmap (C.Image w h _) _ _) = V2 w h
size Blank = zero

liftImage :: C.Image C.PixelRGBA8 -> Bitmap
liftImage b@(C.Image _ _ r) = Bitmap b zero (V.foldl H.hashWithSalt 0 r)

liftImage' :: MonadIO m => C.Image C.PixelRGBA8 -> m Bitmap
liftImage' b = liftIO $ Bitmap b zero <$> randomIO

-- | Load an image file.
readFile :: MonadIO m => FilePath -> m Bitmap
readFile path = liftIO $ Bitmap <$> C.readImageRGBA8 path <*> pure zero <*> randomIO

-- | Save 'Bitmap' into a file.
writeFile :: MonadIO m => FilePath -> Bitmap -> m ()
writeFile path (Bitmap p _ _) = liftIO $ C.writePng path p
writeFile _ Blank = fail "Blank bitmap"