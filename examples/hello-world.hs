{-# LANGUAGE Rank2Types, LambdaCase, FlexibleContexts #-}
-- $ ghc -threaded hello-world.hs
import Call
import Call.Util.Deck
import Control.Lens
import Control.Monad.State.Strict
import qualified Call.Util.Text as Text

-- calculates the root-mean-square value of the playing sound
currentRMS :: MonadState Deck m => Int -> m (V2 Float)
currentRMS n = do
  Source s <- use source
  t <- use pos
  let t0 = t - fromIntegral (n-1)/44100
  return $ fmap realToFrac
    $ fmap (/fromIntegral n)
    $ sum $ map (fmap (^(2::Int)) . s) [t0, t0 + 1 / 44100..t]

main = runSystemDefault $ do
  setTitle "Hello, world!"
  src <- readWAVE "examples/hello-world.wav"
  deck <- new $ variable $ Call.Util.Deck.empty & source .~ sampleSource src
  text <- Text.simple defaultFont 32

  linkAudio $ ((deck .-) .) . playback
  linkPicture $ \_ -> do
    let s x = (10 + max (log (realToFrac x) / log 10) (-10)) / 10
    V2 a b <- fmap (fmap s) $ deck .- currentRMS 1024
    return $ translate (V2 0 240) $ mconcat
      [color black $ polygonOutline [V2 0 0, V2 40 0, V2 40 (-240), V2 0 (-240)]
      ,color red $ mconcat [polygonOutline [V2 0 0, V2 16 0, V2 16 (-a * 240), V2 0 (-a * 240)]
      ,polygonOutline [V2 24 0, V2 40 0, V2 40 (-b * 240), V2 24 (-b * 240)]]
      ,translate (V2 40 40) $ color black $ text "Hello, world"
      ]
  linkKeyboard $ \case
    Down KeySpace -> deck .- pos .= 0
    _ -> return ()
  deck .- playing .= True
  stand
 