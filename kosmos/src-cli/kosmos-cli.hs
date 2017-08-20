{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}

import qualified Kosmos
import Riga
import Reflex.Dom
import qualified Data.Text as Text

utterance g x =
  el "ul" $ do
    let xs = map (Kosmos.capitalize . (++ "."))
               (filter (not . null)
                (Kosmos.linearizeAll g (gf x)))
    mapM_ (el "li" . text . Text.pack) xs

main :: IO ()
main = do
  pgf <- Kosmos.fetchGrammar "lastadija.pgf"
  Kosmos.main pgf >>= \case
    Left _ -> mainWidget $ el "div" $ text "Loading failed."
    Right (cores, facts) -> do
      putStrLn $ "Loaded " ++ show (length cores) ++ " cores"
      let exploration = Kosmos.explore facts cores
      mapM_ (\(core, _) -> print (show core)) exploration
      mainWidget $ do
        el "div" $
          mapM_ (utterance pgf) (Kosmos.relevantFacts facts)
