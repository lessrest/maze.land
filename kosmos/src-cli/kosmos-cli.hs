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
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHCJS.Marshal (toJSValListOf)
import Reflex.Dom
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Kosmos
import Kosmos
  ( Game (..)
  , relevantFacts
  , explore
  , expand
  , abridge
  , capitalize
  , linearizeAll
  , coreDeed
  , playerSpot
  , minimap
  , flyTo
  , fetchBytes
  , parseSpot
  , tryTo
  , factLine
  , fireResizeEvent
  )

type Grammar = Kosmos.PGF

main :: IO ()
main = do
  g <- fetchGrammar
  tales <- fetchTales "tales.txt"

  let tales' = Map.fromList . catMaybes . map f . Map.toList $ tales
      f (k, v) =
        case parseSpot g (Text.unpack k) of
          Nothing -> Nothing
          Just k' -> Just (k', v)
  mapM_ print (map fst $ Map.toList tales')

  Kosmos.start g >>= \case
    Left _ ->
      showLoadingFailed

    Right (Game genesis) -> do
      mainWidgetWithHead
        (do elAttr "script"
              (("defer" =: "defer")
               <> ("src" =: "maze.js"))
              blank
            elAttr "script"
              (("defer" =: "defer")
               <> ("src" =: "https://api.mapbox.com/mapbox-gl-js/v0.38.0/mapbox-gl.js"))
              blank
            elAttr "link"
              (("href" =: "https://api.mapbox.com/mapbox-gl-js/v0.38.0/mapbox-gl.css")
                <> ("rel" =: "stylesheet"))
              blank
            elAttr "link"
              (("href" =: "font.css")
                <> ("rel" =: "stylesheet"))
              blank
            ignore (elDynHtml' "style" (pure (decodeUtf8 css))))
        (do play g genesis tales')

  where
    play :: forall t. Grammar -> [GFact] -> Map GSpot [Text] -> Widget t ()
    play g genesis tales =
      el "app" $ do
        rec
          -- The world is a dynamic list of currently true facts
          world <- holdDyn genesis future
          -- The diary is a list of the current and past worlds
          diary <- foldDyn (:) [genesis] (updated world)
          -- The lingo is a dynamic number representing the chosen translation
          lingo <- _dropdown_value <$> dropdown 0 (pure lingos) def
          -- The options are a dynamic list of possible world transitions
          let options = (explore . expand) <$> world

          let saySpot x = (translate g x) <$> lingo

          liftIO (toJSValListOf (map (snd . snd) allData) >>= minimap)

          performEvent $
            fmap (\x -> liftIO (maybe (pure ()) (uncurry flyTo) x))
              (updated (nubDyn (fmap (fmap snd . join . fmap dataFor . playerSpot) world)))

          elDynAttr "video" (worldVideoAttrs <$> world) blank
          el "vignette" blank

          (x, bigMinimap) <- elDynAttr' "minimap-screen" (bigMinimapAttrs <$> bigMinimap) $ do
            el "minimap" blank
            el "minimap-glass" blank
            toggle False (domEvent Click x)

          performEvent $
            fmap (\x -> liftIO (maybe (pure ()) (uncurry flyTo) x))
              (tag (current (fmap (fmap snd . join . fmap dataFor . playerSpot) world)) (updated bigMinimap))

          el "place" . dynText $
            join $ fmap (maybe (pure "?") saySpot) $ fmap playerSpot world

          let tale = fmap (fmap (maybe [] id . flip Map.lookup tales) . playerSpot) world

          future <- el "rulebox" $ mdo
            showRelevantFacts g lingo (fmap (abridge . snd . expand) world)
            -- el "tale" $
            --   simpleTextList (fmap (maybe [] id) tale)
            -- showOptions g lingo options
            input <- textInput $ def
              & textInputConfig_setValue .~ fmap (const "") (textInputGetEnter input)
              & textInputConfig_attributes .~ pure (mconcat ["placeholder" =: "What to do?", "autofocus" =: "autofocus"])
            return $
              tag (fmap (tryTo g) ((,) <$> current world <*> current (value input)))
                  (textInputGetEnter input)

        blank

    showLoadingFailed = mainWidgetWithCss css (el "div" $ text "Loading failed.")

    showRelevantFacts g lingo world = do
      ignore . simpleList (fmap (map factLine . relevantFacts) world) $ \x -> do
        el "p" $ dynText (fmap (<> ".") ((translate g) <$> x <*> lingo))

    showOptions g lingo options = fmap (switch . current . fmap leftmost) x
      where
        x = do el "ul" $ simpleList options $ \option -> do
                 el "li" $ do
                   (b, _) <- el' "button" $ do
                     -- Show the rule for this option
                     dynText $
                       ((\x i -> maybe "?" (\y -> translate g (GTryTo y) i) x)
                         <$> (fmap (coreDeed . fst) option) <*> lingo)
                   -- When the button is clicked, tag the event
                   return $ tag (snd <$> current option) (domEvent Click b)

translate :: (Show a, Gf a) => Grammar -> a -> Int -> Text
translate g x i =
  let xs = inAllLanguages g x in
    if length xs >= i + 1
    then xs !! i
    else pack (show x)

inAllLanguages :: Gf a => Grammar -> a -> [Text]
inAllLanguages g =
  ( map (pack . capitalize)
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

fetchTales :: String -> IO (Map Text [Text])
fetchTales path = do
  txt <- decodeUtf8 <$> fetchBytes path
  return (parse1 mempty (filter (/= "") (Text.lines txt)))
  where
    parse1 a [] =
      a
    parse1 a (x:xs) | Text.isPrefixOf "** " x =
      parse2 a (Text.drop 3 x) [] xs
    parse1 a (x:xs) =
      parse1 a xs
    parse2 a name tale [] =
      a <> Map.singleton name (reverse tale)
    parse2 a name tale (x:xs) | Text.isPrefixOf "** " x =
      parse2 (a <> Map.singleton name (reverse tale)) (Text.drop 3 x) [] xs
    parse2 a name tale (x:xs) =
      parse2 a name (x:tale) xs

type Coords = (Float, Float)

a %% b = (a, b)

dataFor :: GSpot -> Maybe (Text, Coords)
dataFor x = fmap snd (find ((== x) . fst) allData)

allData :: [(GSpot, (Text, Coords))]
allData =
  [ ( Gspot_TheCentralMarketStands
    , ("98878fc4a028e89abac95f6f5556cdb9ca18730b5f1dfa36e84d0c40b96c4594", 56.94433 %% 24.11642))
  , ( Gspot_TheNightMarket
    , ("ebb3d0c36c74462067e6412016f4525e8972e751e3d25e445c7f71c42d611de5", 56.94348 %% 24.11753))
  , ( Gspot_TheFreeRigaNeighborhoodResidence
    , ("a701b6b2a7fe456188f35cefa96afbd84cdc0f173b34b0737a5c3fbccf8593f8", 56.94233 %% 24.12010))
  , ( Gspot_TheSpikeriPromenade
    , ("ef7b405b5dc708a4c7dc9ab5cd085d31075fbc222185dd9241d163f00ca85847", 56.94143 %% 24.11477))
  , ( Gspot_Banuzis
    , ("6f71d3a119e107cf507a4e3a05e2417f0d6daf2227a705cac755aedad00d6ab4", 56.94317 %% 24.12433))
  , ( Gspot_TheScienceAcademy
    , ("6b254751e4f0be9515a460762daeb9007469b61667793e714e9bade62288ce36", 56.94356 %% 24.12101))
  , ( Gspot_TheSynagogueMemorial
    , ("d4c5672b8ba26ec282676be3adfc051c775e4a94f816f9fa64f99f6b69eaee7d", 56.94269 %% 24.12632))
  , ( Gspot_TheJesusChurch
    , ("005f36c1f45e03f1bae2efb351257bcb204920fb47b2cfd265d95a0fc9013007", 56.94183 %% 24.12400))
  , ( Gspot_TheBookShopsJanusAndGora
    , ("309bbaee44abdede91b689328c8722d03ff2185eb38aa4bf1463a88836a73f61", 56.94108 %% 24.12280))
  , ( Gspot_SviestaPika
    , ("6d9f3513bc6896af69c9ac84cfeee8ccfc6105d98676af2bd8527208be402abc", 56.94232 %% 24.12171))
  , ( Gspot_Deficits
    , ("72a3f7c6d4487513ca521b7f6f159c77cc75b9995602edda7490ea14f7406166", 56.94309 %% 24.12450))
  , ( Gspot_TheMilkPavilion
    , ("0538876c89055db0b2fd7f6ffdd4e54b870d314c42621518eadc91f729319712", 56.94420 %% 24.11600))
  , ( Gspot_TheFishPavilion
    , ("f689c61ce4936b2e44cbb26f19c362a26675e600af8ed85829913bf8470a01dc", 56.94336 %% 24.11382))
  , ( Gspot_TheVegetablePavilion
    , ("9ed815b635da1a885f68d8e289535d35afbdceb930bbd76456bf0950d8847a56", 56.94362 %% 24.11441))
  , ( Gspot_OutsideTheUzbekPlace
    , ("ae245ece7da6e3d247aad8dd26820383cb9e1049177d1fbe4d4d198b6b691dc7", 56.94304 %% 24.11411))
  , ( Gspot_TheEmptyPavilion
    , ("88d0a1019e311b66977a48f59b9c202eeeedd2b180f6a23d02ff9f1185612e2e", 56.94381 %% 24.11519))
  , ( Gspot_TheBusStation
    , ("199db25cc396b605b3bf78e0b0179709d8e9b13384c790cb82ce4d584cdd2017", 56.94478 %% 24.11333))
  , ( Gspot_Agroprojekts
    , ("6b9e84ed856d666220c8a8b98dc88501eb241d858c6e8dc82a0455dcc9984fb2", 56.93965 %% 24.12275))
  , ( Gspot_CafeRosemary
    , ("146197ac1d66f1612b2e3d72c3541f5804c82b3cee0f29fd2f9ce9f50909fd75", 56.93980 %% 24.12587))
  , ( Gspot_DagdasStreet
    , ("cbeaf4f7269a774f823e66a31c40048106435eff0bcc784e674fd68cd05ad08d", 56.94147 %% 24.12937))
  , ( Gspot_GoodwillStudio
    , ("", 56.94273 %% 24.12459))
  , ( Gspot_Idioma
    , ("", 56.94136 %% 24.11967))
  , ( Gspot_Latgalite
    , ("ef35dc0c58bfea4a478400d1be56647c3e6c602a209f8019ab7b3b4e40036e1e", 56.94262 %% 24.12897))
  , ( Gspot_MazaKrastaStreet
    , ("cdcadd298bbb533bcd6b1bda53f0e9b27afd35129e57311e608c8d4d384c3a30", 56.93930 %% 24.12454))
  , ( Gspot_Pushkin11
    , ("", 56.94202 %% 24.12103))
  , ( Gspot_SpekaStreet
    , ("6e7d5eace8208cd97898b439f1b2193b91d266585ca8e757b73858bca0270b70", 56.93962 %% 24.12666))
  , ( Gspot_Spikeri
    , ("", 56.94211 %% 24.11560))
  , ( Gspot_TheAbrenesStreetBusStation
    , ("30bd1448d625bed9c44d07419aa1ac5d900da252bed535744a6970736be1b595", 56.94597 %% 24.13090))
  , ( Gspot_TheCentralMarketShoemaker
    , ("", 56.94461 %% 24.11880))
  , ( Gspot_TheDaugavaSwimmingSpot
    , ("", 56.94265 %% 24.11171))
  , ( Gspot_TheGhettoMuseum
    , ("", 56.94110 %% 24.11703))
  , ( Gspot_TheGreenField
    , ("ef08484983a2f8b5312d8432f153839d8e91ba5d7652cdb968e76fc0ab66532b", 56.94174 %% 24.12033))
  , ( Gspot_TheOrthodoxChurch
    , ("", 56.94398 %% 24.12308))
  , ( Gspot_ThePan
    , ("c56a143ab7d60e761e3eef5f211ccaf43c4aefa2fc3944981f5020730e9ffb42", 56.94581 %% 24.12068))
  , ( Gspot_ThePushkinStreetPrintShop
    , ("", 56.94154 %% 24.12129))
  , ( Gspot_TheRedCrossSocialCenter
    , ("", 56.94249 %% 24.11656))
  , ( Gspot_TheRiversideGallery
    , ("", 56.94064 %% 24.11788))
  , ( Gspot_TheSoyShop
    , ("", 56.94350 %% 24.12365))
  , ( Gspot_TheSpikeriPromenade
    , ("", 56.94141 %% 24.11468))
  , ( Gspot_TheTunnel
    , ("66c0ba38ff0150793cf1097ef09ecd8eb4a0f317006fff40e02c1dfe3c1c8ff7", 56.93923 %% 24.12852))
  , ( Gspot_TheVeraMuhinasMemorialHouse
    , ("ddbc9b681e36ec53f8effdc3e3657934ba9449ed141e2b0f1b9bb592311602fe", 56.94515 %% 24.12429))
  , ( Gspot_VingrumaClub
    , ("a87e849b4832d8d55d716a33cbe74efeed522efdad36a3bbb69f2a1fc8efb7db", 56.94116 %% 24.12716))
  , ( Gspot_TheIndustrialGoodsMarket
    , ("", 56.94288 %% 24.11910))
  , ( Gspot_TheHummusTeam
    , ("", 56.94182 %% 24.12281))
  , ( Gspot_DzirnavuTurgeneva
    , ("411fe05919a18bdd215949784093a2d1f3146f2598a917661188ad9db247242c", 56.94596 %% 24.12570))
  , ( Gspot_RedHouse
    , ("67a34e6894df0bee39f7e70686c7aa778f0c78f981d9fbae7265ac268a1bb820", 56.93974 %% 24.12210))
  ]

worldVideoAttrs xs =
  case playerSpot xs of
    Nothing -> mempty
    Just spot ->
      case dataFor spot of
        Nothing -> mempty
        Just (clip, _) -> mconcat
          [ "src" =: ("https://d2ayo97fkylvct.cloudfront.net/" <> clip <> "/1080p-vp9/clip.webm")
          , "loop" =: "loop"
          , "autoplay" =: "autoplay"
          ]

bigMinimapAttrs False = mempty
bigMinimapAttrs True = "class" =: "big"

css = encodeUtf8 . pack . unlines $
  [ "video { height: 400px }"
  , "html, body, button, select, input { font: " ++ font ++ " }"
  , "html, body { position: absolute; top: 0; left: 0; right: 0; bottom: 0; }"
  , "html, body { margin: 0 }"
  , "article { min-height: 100vh; min-width: 100vh; }"
  , "video { position: fixed; top: 50%; left: 50%; }"
  , "video { min-width: 100%; min-height: 100%; width: auto; height: auto }"
  , "video { transform: translateX(-50%) translateY(-50%) }"
  , "rulebox { position: absolute; bottom: 2em; right: 2em; }"
  , "rulebox { max-width: 32em; padding: 0 1em } "
  , "rulebox { transform: rotate(2deg) }"
  , "select { position: absolute; right: 1em; top: 1em; z-index: 1 }"
  , "select { padding: .25em .5em }"
  , "button { padding: 0 .5em }"
  , "ul { list-style-type: none; padding: 0 }"
  , "place { position: absolute; left: 1em; top: 1em }"
  , "place { padding: .25rem .5rem; }"
  , "place { color: ivory; font-weight: bold; }"
  , "place { transform: rotate(-2deg); }"
  , "place { font-size: 1.25rem; border-radius: 6px; }"
  , "minimap { display: block; opacity: 0.9 }"
  , "minimap, minimap-screen { height: 400px; width: 400px; z-index: 1 }"
  , "minimap, minimap-screen { border-radius: 1em; }"
  , "minimap-screen { position: absolute !important; bottom: 2em; left: 2em }"
  , "minimap-glass { position: relative; top: -100%; height: 100%; }"
  , "minimap-glass { display: block; opacity: 0.4; z-index: 2 }"
  , "minimap-glass { background-image: url(\"https://www.transparenttextures.com/patterns/little-pluses.png\") }"

  , ".marker { height: 10px; width: 10px; background: yellow; border-radius: 100% }"
  , ".marker { border: 1px solid rgba(0, 0, 0, 0.5) }"
  , "vignette { position: relative; display: block; height: 100%; top: -100%; }"
  , "vignette { background: radial-gradient(transparent, rgba(0, 0, 0, 0.3)) }"
  , "vignette { pointer-events: none }"
  , "rulebox { text-align: justify; max-width: 28em; }"
  , "tale li:not(:first-of-type) { display: none; }"
  , "rulebox { background: rgba(0, 0, 0, 0.8); color: ivory; }"
  , "rulebox { border-radius: .5rem }"
  , "rulebox input { width: 100%; margin-bottom: 1rem; padding: .25rem .5rem}"
  , "rulebox input { background: rgba(0, 0, 0, 0); color: ivory; }"
  , "rulebox input { border: 1px solid currentcolor; }"
  , "* { box-sizing: border-box }"
  , "minimap-screen.big, minimap-screen.big minimap { height: 800px; width: 1000px; }"
  ]

font = "22px/32px \"fantasque sans mono\""

lingos :: Map Int Text
lingos = Map.fromList
  [ (0, "English")
  -- , (1, "Latviski (gramatika)")
  -- , (2, "Latviski")
  , (1, "Svenska")
  ]
