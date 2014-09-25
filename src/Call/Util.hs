module Call.Util where
import Control.Monad.Objective.Class
import Control.Object
import Call.Component
import Call.Component.Deck
import Call.System
import Call.Types
import Call.Data.Wave
import Call.Picture
import Control.Lens

animate :: (Time -> System s (Picture ())) -> System s (Address PullGraphic (System s))
animate f = do
  p <- new (go 0)
  linkGraphic p
  return p
  where
    go t = Object $ \(PullGraphic dt cont) -> f t >>= \p -> return (cont p, go (t + dt))

withSound :: Source Stereo -> System s a -> System s a
withSound src m = do
  deck <- new emptyDeck
  deck .& source ?= src
  deck .& playing .= True
  linkAudio deck
  r <- m
  unlinkAudio deck
  return r