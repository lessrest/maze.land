{-# Language CPP #-}
{-# Language TupleSections #-}
{-# Language OverloadedStrings #-}

#ifdef __GHCJS__
{-# Language JavaScriptFFI #-}
#endif

module Kosmos
  ( start
  , Game (..)
  , PGF
  , PGF.linearizeAll
  , fetchGrammar
  , fetchBytes
  , explore
  , expand
  , abridge
  , relevantFacts
  , capitalize
  , coreDeed
  , playerSpot
  , minimap
  , flyTo
  , fireResizeEvent
  , parseSpot
  , tryTo
  , factLine
  ) where

import Debug.Trace

import Riga
import PGF (PGF)
import qualified PGF as PGF
import qualified PGF.Internal

import Data.Foldable (Foldable, foldMap, toList)
import Control.Monad (msum, guard)
import Data.Maybe (mapMaybe, fromJust, isJust)
import PGF.Lexing (capitInit)
import Data.Monoid (First (..))

import qualified Data.List as List
import Data.List (find, elem)

#ifdef __GHCJS__
import GHCJS.Foreign.Callback
import GHCJS.Types (JSVal)
import Data.JSString ()
import qualified Data.JSString as JS
import qualified Data.JSString.Text as JS
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Base64 as BS64

foreign import javascript interruptible
  "fetchBase64($1, $c);"
  fetchBase64 :: JS.JSString -> IO JSVal

foreign import javascript
  "minimap($1);"
  minimap :: JSVal -> IO ()

foreign import javascript
  "flyTo($1, $2);"
  flyTo :: Float -> Float -> IO ()

foreign import javascript
  "mymap.resize();"
  fireResizeEvent :: IO ()

#endif

-- Some initial assumptions using the "Riga" game grammar.
example1 :: PGF -> [GFact]
example1 g = grok g
  [ "Terapija is north from the central market"
  , "four big watermelons are in the central market"
  , "rule: when the player is in the central market "
      ++ "if you spend one euro then you get one watermelon"
  , "the player is in the central market"
  , "you have three euros"
  ]

lastadijaSpots :: PGF -> IO [GFact]
lastadijaSpots g =
  grok g . lines <$> readFile "../lastadija.txt"

data Core1 = Keep GFact | Take GFact | Give GFact
  deriving (Ord, Eq, Show)

type Core = [Core1]

-- Try to apply a core rule to a list of assumptions,
-- producing (when possible) a new list of assumptions.
apply :: [GFact] -> Core -> Maybe [GFact]
apply facts = \case
  Keep fact : more ->
    do guard (elem fact facts)
       apply facts more
  Take fact : more ->
    do guard (elem fact facts)
       apply (List.delete fact facts) more
  Give fact : more ->
    apply (infer fact ++ facts) more
  [] -> Just facts

-- Produce all possible outcomes from applying any one core rule
-- to a list of assumptions.
explore :: ([Core], [GFact]) -> [(Core, [GFact])]
explore (cores, premises) =
  mapMaybe (\x -> fmap (x,) (apply premises x)) cores

-- Find the facts pertaining to the player's current situation.
relevantFacts :: [GFact] -> [GFact]
relevantFacts facts =
  case List.find (\case { GSpotHasItem spot GPlayer -> True ; _ -> False }) facts of
    Just (GSpotHasItem spot GPlayer) ->
      let p (GSpotHasItem x _)   | x == spot = True
          p (GSpotHasDoor _ _ x) | x == spot = True
          p (GYouHaveItem _) = True
          p _ = False
      in filter p facts
    _ -> []

factLine :: GFact -> GLine
factLine = \case
  GSpotHasDoor dst how _ ->
    GDoorLine how dst
  x ->
    GFactLine x

infer :: GFact -> [GFact]
infer = \case
  -- Reverse doors.
  GSpotHasDoor src how dst ->
    [ GSpotHasDoor src how dst
    , GSpotHasDoor dst (back how) src ]
  x ->
    [x]

core1 :: GCore1 -> Core1
core1 = \case
  GFactAcquisition x -> Give x
  GFactConsumption x -> Take x
  GFactPresumption x -> Keep x
  GItemAcquisition x -> Give (GYouHaveItem x)
  GItemConsumption x -> Take (GYouHaveItem x)
  GItemPresumption x -> Keep (GYouHaveItem x)

-- Expand syntactic facts and extract core rules.
-- (We should probably also use subtype for "core facts.")
expand :: [GFact] -> ([Core], [GFact])
expand = foldMap $
  \case
    -- Make doors into player movement rules.
    GSpotHasDoor src how dst ->
      ( [[ Take (GSpotHasItem src GPlayer)
         , Give (GSpotHasItem dst GPlayer)
         , Keep (GSpotHasDoor src how dst)
         ]]
      , [GSpotHasDoor src how dst]
      )

    -- -- Expand `while' rules to core rules.
    -- GRuleApplies (GWhileRule fact deed boon) ->
    --   ([ Keep fact :
    --        Keep (GRuleApplies (GWhileRule fact deed boon)) :
    --         (case deed of
    --            GConsumption need ->
    --              Take (GYouHaveItem need) :
    --                (Give (GYouHaveItem boon) : [])
    --            GPresumption need ->
    --              Keep (GYouHaveItem need) :
    --                (Give (GYouHaveItem boon) : []))
    --    ], [GRuleApplies (GWhileRule fact deed boon)])

    rule@(GRuleApplies (GGeneralRule2 a b)) ->
      ([map core1 [a, b]], [rule])

    rule@(GRuleApplies (GGeneralRule3 a b c)) ->
      ([map core1 [a, b, c]], [rule])

    rule@(GRuleApplies (GGeneralRule4 a b c d)) ->
      ([map core1 [a, b, c, d]], [rule])

    -- Expand aggregate items to multiple items.
    GYouHaveItem item ->
      ([], map GYouHaveItem (expandAggregateItem item))

    x -> ([], [x])

expandAggregateItem :: GItem -> [GItem]
expandAggregateItem =
  \case
    GCount GSome2 kind ->
      replicate 2 (GCount GSome1 kind)
    GCount GSome3 kind ->
      replicate 3 (GCount GSome1 kind)
    GCount GSome4 kind ->
      replicate 4 (GCount GSome1 kind)
    GBoth x y ->
      expandAggregateItem x ++ expandAggregateItem y
    x -> [x]

abridge :: [GFact] -> [GFact]
abridge facts =
  let
    (a, b) = List.partition (\case { GYouHaveItem _ -> True; _ -> False }) facts
    a' = map GYouHaveItem (groupIdenticalItems (map (\(GYouHaveItem x) -> x) a))
  in
    a' ++ b

groupIdenticalItems :: [GItem] -> [GItem]
groupIdenticalItems = map f . List.group . List.sort
  where
    f xs =
      case (head xs, length xs) of
        (GCount _ kind, 1) -> GCount GSome1 kind
        (GCount _ kind, 2) -> GCount GSome2 kind
        (GCount _ kind, 3) -> GCount GSome3 kind
        (GCount _ kind, 4) -> GCount GSome4 kind
        (_, _) -> error (show (head xs, length xs))

check :: [GFact] -> Either GFail [GFact]
check xs =
  case msum (map bad xs) of
    Nothing -> Right xs
    Just (src, how1, how2, dst) ->
      Left (GDoorConflict src dst how1 how2)
  where
    bad (GSpotHasDoor src1 how1 dst1) =
      msum $ map (\case
              GSpotHasDoor src2 how2 dst2
                | src1 == src2 && dst1 == dst2 && how1 /= how2
                  -> Just (src1, how1, how2, dst1)
              GSpotHasDoor src2 how2 dst2
                | (src1 == src2 && dst1 /= dst2) && how1 == how2
                  -> error $ "you can go to " ++ show src1 ++ " from both " ++ show dst1 ++ " and " ++ show dst2 ++ " via " ++ show how1
              GSpotHasDoor src2 how2 dst2
                | (src1 /= src2 && dst1 == dst2) && how1 == how2
                  -> error $ "you can come from " ++ show dst1 ++ " via " ++ show how1 ++ " both" ++ show src1 ++ " and " ++ show src2
              _ -> Nothing) xs
    bad _ = Nothing

coreDeed :: Core -> Maybe GDeed
coreDeed = \case
  ([ Keep (GSpotHasItem _ _)
   , Keep (GRuleApplies _)
   , Take (GYouHaveItem a)
   , Give (GYouHaveItem b)
   ]) ->
    Just (GSimpleShoppingDeed b a)

  ([ Take (GSpotHasItem src GPlayer)
   , Give (GSpotHasItem dst GPlayer)
   , Keep (GSpotHasDoor _ how _)
   ]) ->
     Just (GSimpleWalkingDeed (back how) dst)

  _ ->
    Nothing

playerSpot :: [GFact] -> Maybe GSpot
playerSpot world =
  case List.find p world of
    Just (GSpotHasItem x _) -> Just x
    _ -> Nothing
  where
    p (GSpotHasItem _ GPlayer) = True
    p _ = False

parseSpot :: PGF -> String -> Maybe GSpot
parseSpot g s =
  let parses = PGF.parseAllLang g (fromJust (PGF.readType "Spot")) s
  in case (do (_, trees) <- parses
              map (fg :: PGF.Tree -> GSpot) trees) of
       [x] -> Just x
       _ -> Nothing

parseFact :: PGF -> String -> Maybe GFact
parseFact g s =
  let parses = PGF.parseAllLang g (PGF.startCat g) s
  in case (do (_, trees) <- parses
              map (fg :: PGF.Tree -> GLine) trees) of
    [GFactLine a] -> Just a
    [GRuleLine a] -> Just (GRuleApplies a)
    _   -> Nothing

parseLine :: PGF -> String -> Maybe GLine
parseLine g s =
  let parses = PGF.parseAllLang g (fromJust (PGF.readType "Line")) s
  in case (do (_, trees) <- parses
              map (fg :: PGF.Tree -> GLine) trees) of
       [x] -> Just x
       _ -> Nothing

yell :: (Show a, Gf a) => PGF -> a -> [String]
yell g x =
  show x :
    (map (("- " ++) . capitInit . (++ "."))
      (filter (not . null)
        (PGF.linearizeAll g (gf x))))

back :: GDoor -> GDoor
back = \case
  GNorth -> GSouth
  GSouth -> GNorth
  GWest -> GEast
  GEast -> GWest
  GNorthWest -> GSouthEast
  GNorthEast -> GSouthWest
  GSouthWest -> GNorthEast
  GSouthEast -> GNorthWest

grok :: PGF -> [String] -> [GFact]
grok g ss =
  let xs = map (\x -> (x, parseFact g x)) ss
  in map (\(s, x) -> maybe (error s) id x) $ xs

-- run :: PGF -> [GFact] -> IO ()
-- run g x = do
--   case expand x of
--     Left a -> mapM_ putStrLn (yell g a)
--     Right (a, b) -> do
--       putStrLn "* Cores:\n"
--       yellSet g a
--       putStrLn "* Facts:\n"
--       yellSet g b
--       putStrLn "* Explore:\n"
--       mapM_ (\(x, _) -> putStrLn "* Option\n" >> mapM_ putStrLn (yell g x)) (explore b a)

yellSet :: (Foldable t, Gf a, Show a) => PGF -> t a -> IO ()
yellSet g xs =
  mapM_ ((>> putStrLn "") . mapM_ putStrLn) (map (yell g) (toList xs))

readGrammar :: IO PGF
readGrammar = PGF.readPGF "Riga.pgf"

#if __GHCJS__

fetchBytes :: String -> IO BS.ByteString
fetchBytes url = f <$> fetchBase64 (JS.pack url)
  where
    f = BS64.decodeLenient . Text.encodeUtf8 . JS.textFromJSVal

fetchGrammar :: String -> IO PGF
fetchGrammar x =
  PGF.Internal.decode . fromStrict <$> fetchBytes x

start :: PGF -> IO (Either GFail Game)
start pgf = do
  putStrLn "Haskell: calling JavaScript to fetch PGF..."
  bs2 <- fetchBytes "lastadija.txt"
  putStrLn $ "Haskell: PGF has languages " ++ show (PGF.languages pgf)
  let txt = Text.unpack (Text.decodeUtf8 bs2)
  return $
    ( fmap Game
    . check
    . foldMap infer
    . grok pgf
    . lines
    ) txt

data Game = Game
  { gameFacts :: [GFact] }

#else

main :: IO ()
main = do
  g <- readGrammar
  lastadijaSpots g >>= run g

#endif

capitalize = capitInit

tryTo :: PGF -> ([GFact], Text) -> [GFact]
tryTo g (world, text) = maybe world id x
  where
    x = do GTryTo deed <- parseLine g (Text.unpack text)
           let (cores, facts) = traceShowId $ (expand world)
           getFirst . mconcat . map (First . apply facts) $
             filter (interpret deed) cores

interpret :: GDeed -> Core -> Bool
interpret = \case
  GBuyDeed item -> elem (Give (GYouHaveItem item))
  GEatDeed item -> elem (Take (GYouHaveItem item))
  GSellDeed item -> \x -> elem (Take (GYouHaveItem item)) x && hasSome (\case { Give (GYouHaveItem _) -> True; _ -> False }) x
  GGoDeed door ->
    hasSome $ \case
      Keep (GSpotHasDoor _ x _) -> back x == door
      _ -> False
  GConnectDeed src how dst ->
    elem (Give (GSpotHasDoor dst how src))
  _ -> const False

hasSome :: (a -> Bool) -> [a] -> Bool
hasSome f = isJust . find f
