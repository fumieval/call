module Call.Util.Sampler {-# DEPRECATED "Use AudioVisual.Sampler instead" #-}  (module Audiovisual.Sampler, Sampler) where

import Audiovisual.Sampler hiding (Sampler)
import Data.Audio
import qualified Audiovisual.Sampler

type Sampler = Audiovisual.Sampler.Sampler Stereo
