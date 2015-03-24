module Call.Util.Deck {-# DEPRECATED "Use AudioVisual.Deck instead" #-} (module Audiovisual.Deck, Deck) where

import Audiovisual.Deck hiding (Deck)
import Data.Audio
import qualified Audiovisual.Deck

type Deck = Audiovisual.Deck.Deck Stereo
