{-# LANGUAGE Rank2Types, LambdaCase, FlexibleContexts #-}
-- $ ghc -threaded hello-world.hs
import Call
import Call.Util.Deck
import Control.Lens
import Control.Monad.State.Strict

-- calculates the root-mean-square value of the playing sound
currentRMS :: MonadState Deck m => Int -> m (V2 Float)
currentRMS n = use source >>= \case
  Just (Source s) -> do
    r <- use sampleRate
    t <- use pos
    let t0 = t - fromIntegral (n-1)/r
    return $ fmap realToFrac $ fmap (/fromIntegral n) $ sum $ map (fmap (^(2::Int)) . s) [t0, t0 + 1 / r..t]
  Nothing -> return 0

handle :: Lift (State Deck) e => Address e (System s) -> Object Keyboard (System s)
handle deck = oneshot $ \case
  Request (Down KeySpace) cont -> do
    deck .& pos .= 0
    cont ()
  Request _ cont -> cont ()

-- visualizes the loudness of current sound.
meter :: Lift (State Deck) e => Address e (System s) -> Object Graphic (System s)
meter deck = oneshot $ \(Request _ cont) -> do
  let s x = (10 + max (log (realToFrac x) / log 10) (-10)) / 10
  V2 a b <- fmap (fmap s) $ deck .& currentRMS 1024
  cont $ translate (V2 0 240) $ do
    color black $ do
      polygonOutline [V2 0 0, V2 40 0, V2 40 (-240), V2 0 (-240)]
    color red $ do
      polygonOutline [V2 0 0, V2 16 0, V2 16 (-a * 240), V2 0 (-a * 240)]
      polygonOutline [V2 24 0, V2 40 0, V2 40 (-b * 240), V2 24 (-b * 240)]

main = runSystemDefault $ do
  deck <- new Call.Util.Deck.empty
  linkAudio deck
  new (meter deck) >>= linkGraphic
  new (handle deck) >>= linkKeyboard
  src <- readWAVE "hello-world.wav"
  deck .& source ?= src
  deck .& playing .= True
  stand
 