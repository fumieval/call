{-# LANGUAGE Rank2Types, LambdaCase #-}
import Call
import Call.Component.Deck
import Control.Lens

rms n = use source >>= \case
  Just (Source s) -> do
    r <- use sampleRate
    t <- use pos
    let t0 = t - fromIntegral (n-1)/r
    return $ fmap realToFrac $ fmap (/fromIntegral n) $ sum $ map (fmap (^(2::Int)) . s) [t0, t0 + 1 / r..t]
  Nothing -> return 0

handle deck = oneshot $ \case
  KeyEvent KeySpace True cont -> do
    deck .& pos .= 0
    cont
  KeyEvent _ _ cont -> cont

meter deck = oneshot $ \(PullGraphic _ cont) -> do
  let s x = (10 + max (log x / log 10) (-10)) / 10
  V2 a b <- fmap (fmap s) $ deck .& rms 1024
  cont $ translate (V2 0 240) $ do
    color black $ do
      polygonOutline [V2 0 0, V2 40 0, V2 40 (-240), V2 0 (-240)]
    color red $ do
      polygonOutline [V2 0 0, V2 16 0, V2 16 (-a * 240), V2 0 (-a * 240)]
      polygonOutline [V2 24 0, V2 40 0, V2 40 (-b * 240), V2 24 (-b * 240)]

main = runSystemDefault $ do
  deck <- new emptyDeck
  linkAudio deck
  new (meter deck) >>= linkGraphic
  new (handle deck) >>= linkKeyboard
  src <- readWAVE "hello-world.wav"
  deck .& source ?= src
  deck .& playing .= True
  stand
