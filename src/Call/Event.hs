module Call.Event where
import Control.Object
import Call.Picture
import Call.Types
import Linear.V2

type Graphic = Request Time (Picture ())
type Audio = Request (Time, Int) [V2 Float]
type Mouse = Request MouseEvent ()
type Keyboard = Request (Chatter Key) ()
