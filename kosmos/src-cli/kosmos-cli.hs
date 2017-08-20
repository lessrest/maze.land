{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}

import Riga

import Reflex.Dom
import Data.Text (Text, pack)

import qualified Kosmos
import Kosmos
  ( Game (..)
  , relevantFacts
  , explore
  , expand
  , capitalize
  , linearizeAll
  )

type Grammar = Kosmos.PGF

main :: IO ()
main = do
  g <- fetchGrammar
  Kosmos.start g >>= \case
    Left _ ->
      showLoadingFailed

    Right (Game genesis) -> do
      mainWidget $ do
        rec
          -- The world is a dynamic list of currently true facts
          world <- holdDyn genesis (switch (leftmost <$> current future))
          showRelevantFacts g world

          -- The options are a dynamic list of possible transitions
          let options = (explore . expand) <$> world
          future <- showOptions options

        blank

  where
    showLoadingFailed = mainWidget (el "div" $ text "Loading failed.")

    showRelevantFacts g world = do
      el "h1" (text "Facts")
      ignore . simpleList (fmap relevantFacts world) $ \fact -> do
        simpleTextList (inAllLanguages g <$> fact)

    showOptions options = do
      el "h1" (text "Choices")
      el "ul" $ simpleList options $ \option -> do
        el "li" $ do
          b <- button "Do"
          -- Show the rule for this option
          dynText ((pack . show . fst) <$> option)
          -- When the button is clicked, tag the event
          return (tag (snd <$> current option) b)

inAllLanguages :: Gf a => Grammar -> a -> [Text]
inAllLanguages g =
  ( map (pack . capitalize . (++ "."))
  . filter (not . null)
  . linearizeAll g
  . gf
  )

simpleTextList :: (Reflex t, MonadWidget t m) => Dynamic t [Text] -> m ()
simpleTextList xs =
  ignore . el "ul" $ ignore (simpleList xs (el "li" . dynText))

ignore :: Monad m => m a -> m ()
ignore m = m >> return ()

fetchGrammar :: IO Grammar
fetchGrammar = Kosmos.fetchGrammar "lastadija.pgf"
