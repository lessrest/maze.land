{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}
{-# Language Rank2Types #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}

import Riga

import Control.Arrow ((>>>))
import Control.Monad (join)
import Data.Monoid

import Reflex.Dom
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Kosmos
import Kosmos
  ( Game (..)
  , relevantFacts
  , explore
  , expand
  , capitalize
  , linearizeAll
  , coreDeed
  , playerSpot
  )

type Grammar = Kosmos.PGF

main :: IO ()
main = do
  g <- fetchGrammar
  Kosmos.start g >>= \case
    Left _ ->
      showLoadingFailed

    Right (Game genesis) -> do
      mainWidgetWithCss css $ play g genesis

  where
    play :: forall t. Grammar -> [GFact] -> Widget t ()
    play g genesis =
      el "app" $ do
        rec
          -- The world is a dynamic list of currently true facts
          world <- holdDyn genesis (switch (leftmost <$> current future))
          -- The lingo is a dynamic number representing the chosen translation
          lingo <- _dropdown_value <$> dropdown 0 (pure lingos) def
          -- The options are a dynamic list of possible world transitions
          let options = (explore . expand) <$> world

          let
            saySpot x = (\i -> inAllLanguages g x !! i) <$> lingo

          elDynAttr "video" (worldVideoAttrs <$> world) blank
          el "place" . dynText $
            join $ fmap (maybe (pure "?") saySpot) $ fmap playerSpot world

          future <- el "div" $ do
            -- showRelevantFacts g lingo world
            showOptions g lingo options

        blank

    showLoadingFailed = mainWidgetWithCss css (el "div" $ text "Loading failed.")

    showRelevantFacts g lingo world = do
      ignore . simpleList (fmap relevantFacts world) $ \fact -> do
        el "p" $ dynText ((translate g) <$> fact <*> lingo)

    showOptions g lingo options = do
      el "ul" $ simpleList options $ \option -> do
        el "li" $ do
          b <- button "Do"
          -- Show the rule for this option
          dynText $
            ((\x i -> maybe "?" (\y -> translate g (GDeedWish y) i) x)
              <$> (fmap (coreDeed . fst) option) <*> lingo)
          -- When the button is clicked, tag the event
          return (tag (snd <$> current option) b)

translate :: Gf a => Grammar -> a -> Int -> Text
translate g x i = inAllLanguages g x !! i

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

clipFor :: GSpot -> Maybe Text
clipFor = \case
  Gspot_TheCentralMarketStands ->
    Just "98878fc4a028e89abac95f6f5556cdb9ca18730b5f1dfa36e84d0c40b96c4594"
  Gspot_TheNightMarket ->
    Just "ebb3d0c36c74462067e6412016f4525e8972e751e3d25e445c7f71c42d611de5"
  Gspot_TheFreeRigaNeighborhoodResidence ->
    Just "a701b6b2a7fe456188f35cefa96afbd84cdc0f173b34b0737a5c3fbccf8593f8"
  Gspot_TheSpikeriPromenade ->
    Just "ef7b405b5dc708a4c7dc9ab5cd085d31075fbc222185dd9241d163f00ca85847"
  Gspot_Banuzis ->
    Just "6f71d3a119e107cf507a4e3a05e2417f0d6daf2227a705cac755aedad00d6ab4"
  Gspot_TheScienceAcademy ->
    Just "6b254751e4f0be9515a460762daeb9007469b61667793e714e9bade62288ce36"
  Gspot_TheSynagogueMemorial ->
    Just "d4c5672b8ba26ec282676be3adfc051c775e4a94f816f9fa64f99f6b69eaee7d"
  Gspot_TheJesusChurch ->
    Just "005f36c1f45e03f1bae2efb351257bcb204920fb47b2cfd265d95a0fc9013007"
  Gspot_TheBookShopsJanusAndGora ->
    Just "309bbaee44abdede91b689328c8722d03ff2185eb38aa4bf1463a88836a73f61"
  Gspot_SviestaPika ->
    Just "6d9f3513bc6896af69c9ac84cfeee8ccfc6105d98676af2bd8527208be402abc"
  Gspot_Deficits ->
    Just "72a3f7c6d4487513ca521b7f6f159c77cc75b9995602edda7490ea14f7406166"
  Gspot_TheMilkPavilion ->
    Just "0538876c89055db0b2fd7f6ffdd4e54b870d314c42621518eadc91f729319712"
  Gspot_TheFishPavilion ->
    Just "f689c61ce4936b2e44cbb26f19c362a26675e600af8ed85829913bf8470a01dc"
  Gspot_TheVegetablePavilion ->
    Just "9ed815b635da1a885f68d8e289535d35afbdceb930bbd76456bf0950d8847a56"
  Gspot_OutsideTheUzbekPlace ->
    Just "ae245ece7da6e3d247aad8dd26820383cb9e1049177d1fbe4d4d198b6b691dc7"
  Gspot_TheEmptyPavilion ->
    Just "88d0a1019e311b66977a48f59b9c202eeeedd2b180f6a23d02ff9f1185612e2e"
  Gspot_TheBusStation ->
    Just "199db25cc396b605b3bf78e0b0179709d8e9b13384c790cb82ce4d584cdd2017"

  _ ->
    Nothing

worldVideoAttrs xs =
  case playerSpot xs of
    Nothing -> mempty
    Just spot ->
      case clipFor spot of
        Nothing -> mempty
        Just clip -> mconcat
          [ "src" =: ("https://d2ayo97fkylvct.cloudfront.net/" <> clip <> "/1080p-vp9/clip.webm")
          , "loop" =: "loop"
          , "autoplay" =: "autoplay"
          ]

css = encodeUtf8 . pack . unlines $
  [ "video { height: 400px }"
  , "html, body, button, select, input { font: " ++ font ++ " }"
  , "html, body { position: absolute; top: 0; left: 0; right: 0; bottom: 0; }"
  , "html, body { margin: 0 }"
  , "article { min-height: 100vh; min-width: 100vh; }"
  , "video { position: fixed; top: 50%; left: 50%; }"
  , "video { min-width: 100%; min-height: 100%; width: auto; height: auto }"
  , "video { transform: translateX(-50%) translateY(-50%) }"
  , "div { position: absolute; bottom: 2em; right: 2em; }"
  , "div { max-width: 32em; padding: 0 1em } "
  , "div { background: rgba(0, 0, 0, 0.9); color: ivory }"
  , "div { border: 3px solid yellow; border-radius: 6px; }"
  , "div { transform: rotate(2deg) }"
  , "select { position: absolute; right: 1em; top: 1em; z-index: 1 }"
  , "select { padding: .25em .5em }"
  , "button { margin-right: 1em }"
  , "ul { list-style-type: none; padding: 0 }"
  , "place { position: absolute; left: 1em; top: 1em }"
  , "place { padding: .25rem .5rem; }"
  , "place { background: rgba(255, 255, 255, 0.9); color: black }"
  , "place { transform: rotate(-2deg); border: 3px solid black }"
  , "place { font-size: 1.5rem; border-radius: 6px; }"
  ]

font = "22px/32px \"fantasque sans mono\""

lingos :: Map Int Text
lingos = Map.fromList
  [ (0, "English")
  , (1, "Latviski")
  , (2, "Svenska")
  ]
