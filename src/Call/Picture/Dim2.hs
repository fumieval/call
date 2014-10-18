module Call.Picture.Dim2 where
import Control.Monad.Free.Church

data Base2D a = RotateOn Double (Picture2D a)
  | Scale Vec2 (Picture2D a)
  | Translate Vec2 (Picture2D a)
  | Line [Vec2] a
  | Polygon [Vec2] a
  | PolygonOutline [Vec2] a
  | Circle Double a
  | CircleOutline Double a
  | BitmapToward Double Bitmap a
  | PolygonWith Bitmap [(V2 Double, Vec2)] -> a
  | Thickness Float (Picture a)
  | Color Color (Picture a)
  | BlendMode BlendMode (Picture a)

type Picture2D = F Base2D

instance Affine (Picture2D a) where
  type Vec = Vec2
  type Normal = Double
  rotateOn n = hoistF (RotateOn n)
  scale t = hoistF (Scale t)
  translate t = hoistF (Translate t)

instance Picture (Picture2D ()) where
  line vs = liftF (Line vs ())
  polygon vs = liftF (Polygon vs ())
  polygonOutline vs = liftF (PolygonOutline vs ())
  circle n = liftF (Circle n ())
  circleOutline n = liftF (CircleOutline n ())
  bitmapToward n b = liftF (BitmapToward n b ())
  polygonOutline b vs = liftF (PolygonWith b vs ())

instance Decorate (Picture2D a) where
  color c = hoistF (Color c)
  thickness t = hoistF (Thickness t)
  