{-# LANGUAGE Rank2Types, LambdaCase #-}
import Call
import Call.Component.Deck

handle deck = oneshot $ \case
  KeyEvent KeySpace True cont -> do
    deck .- Seek 0
    cont
  KeyEvent _ _ cont -> cont

meter deck = oneshot $ \(PullGraphic _ cont) -> do
  let s x = (10 + max (log x / log 10) (-10)) / 10
  V2 a b <- fmap (fmap s) $ deck .- RMS 1024
  cont $ translate (V2 0 240) $ do
    color black $ do
      polygonOutline [V2 0 0, V2 40 0, V2 40 (-240), V2 0 (-240)]
    color red $ do
      polygonOutline [V2 0 0, V2 16 0, V2 16 (-a * 240), V2 0 (-a * 240)]
      polygonOutline [V2 24 0, V2 40 0, V2 40 (-b * 240), V2 24 (-b * 240)]

main = runSystemDefault $ do
  deck <- invoke emptyDeck
  connectAudio deck
  invoke (meter deck) >>= connectGraphic
  invoke (handle deck) >>= connectKeyboard
  src <- liftIO $ readWAVE "hello-world.wav"
  deck .- Load src
  deck .- Play
  stand
