{-# Language CPP #-}
{-# Language OverloadedStrings #-}

#ifdef __GHCJS__
{-# Language JavaScriptFFI #-}
#endif

module Kosmos where

import Riga
import PGF (PGF)
import qualified PGF as PGF
import qualified PGF.Internal

import Data.Foldable (Foldable, foldMap, toList)
import Control.Monad (msum, guard)
import Data.Maybe (mapMaybe)
import PGF.Lexing (capitInit)

import qualified Data.List as List

#ifdef __GHCJS__
import GHCJS.Foreign.Callback
import GHCJS.Types (JSVal)
import Data.JSString ()
import qualified Data.JSString as JS
import qualified Data.JSString.Text as JS
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base64 as BS64

foreign import javascript unsafe
  "fetchBase64($1, $2)"
  fetchBase64 :: JS.JSString -> Callback (JSVal -> IO ()) -> IO ()
#endif

-- Some initial assumptions using the "Riga" game grammar.
example1 :: PGF -> [GFact]
example1 g = example g
  [ "Terapija is north from the central market"
  , "four big watermelons are in the central market"
  , "rule: when the player is in the central market "
      ++ "if you spend one euro then you get one watermelon"
  , "the player is in the central market"
  , "you have three euros"
  ]

lastadijaSpots :: PGF -> IO [GFact]
lastadijaSpots g =
  example g . lines <$> readFile "../lastadija.txt"

-- Try to apply a core rule to a list of assumptions,
-- producing (when possible) a new list of assumptions.
apply :: [GFact] -> GCore -> Maybe [GFact]
apply facts = \case
  GKeeping fact more ->
    do guard (elem fact facts)
       apply facts more
  GTaking fact more ->
    do guard (elem fact facts)
       apply (List.delete fact facts) more
  GGiving fact more ->
    apply (fact : facts) more
  GTrivial -> Just facts

-- Produce all possible outcomes from applying any one core rule
-- to a list of assumptions.
explore :: [GFact] -> [GCore] -> [[GFact]]
explore premises = mapMaybe (apply premises)

core :: (GCore -> GCore) -> GCore
core = ($ GTrivial)

-- Expand syntactic facts and extract core rules.
-- (We should probably also use subtype for "core facts.")
expand :: [GFact] -> Either GFail ([GCore], [GFact])
expand = fmap (foldMap stage2) . check . foldMap stage1
  where

    stage1 :: GFact -> [GFact]
    stage1 = \case
      -- Reverse doors.
      GSpotHasDoor src how dst ->
        [ GSpotHasDoor src how dst
        , GSpotHasDoor dst (back how) src ]
      x ->
        [x]

    stage2 :: GFact -> ([GCore], [GFact])
    stage2 = \case
      -- Make doors into player movement rules.
      GSpotHasDoor src how dst ->
        ( [core ( GTaking (GSpotHasItem src GPlayer)
                . GGiving (GSpotHasItem dst GPlayer)
                . GKeeping (GSpotHasDoor src how dst))]
        , [GSpotHasDoor src how dst]
        )

      -- Expand `while' rules to core rules.
      GRuleApplies (GWhileRule fact deed boon) ->
        ([ GKeeping fact
            (GKeeping (GRuleApplies (GWhileRule fact deed boon))
              (case deed of
                 GConsumption need ->
                   GTaking (GYouHaveItem need)
                     (GGiving (GYouHaveItem boon) GTrivial)
                 GPresumption need ->
                   GKeeping (GYouHaveItem need)
                     (GGiving (GYouHaveItem boon) GTrivial)))
         ], [GRuleApplies (GWhileRule fact deed boon)])

      -- Expand aggregate items to multiple items
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
                  -> error $ "you can come from " ++ show dst1 ++ " via " ++ show how1 ++ " ti both" ++ show src1 ++ " and " ++ show src2
              _ -> Nothing) xs
    bad _ = Nothing

-- data Item
--   = One GKind
--   deriving (Eq, Ord, Show)

-- data Fact
--   = Placement GSpot Item
--   | Residence GSpot
--   | Possession Item
--   | Ruling Rule
--   deriving (Eq, Ord, Show)

-- data Need
--   = Consumption Fact
--   | Presumption Fact
--   deriving (Eq, Ord, Show)

-- data Deed
--   = Produce Fact
--   | Consume Fact
--   deriving (Eq, Ord, Show)

-- data Rule
--   = Rule [Need] [Fact]
--   deriving (Eq, Ord, Show)

-- grok :: GFact -> [Fact]
-- grok = \case
--   GSpotHasItem gSpot gItem ->
--     map (Placement gSpot) (grokItem gItem)

--   where
--     grokItem = \case
--       GBoth a b -> grokItem a ++ grokItem b
--       GCount GSome1 x -> [One x]
--       GCount GSome2 x -> [One x, One x]
--       GCount GSome3 x -> [One x, One x, One x]
--       GCount GSome4 x -> [One x, One x, One x, One x]

parseFact :: PGF -> String -> Maybe GFact
parseFact g s =
  let parses = PGF.parseAllLang g (PGF.startCat g) s
  in case (do (_, trees) <- parses
              map (fg :: PGF.Tree -> GLine) trees) of
    [GFactLine a] -> Just a
    [GRuleLine a] -> Just (GRuleApplies a)
    _   -> Nothing

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

example :: PGF -> [String] -> [GFact]
example g ss =
  let xs = map (\x -> (x, parseFact g x)) ss
  in map (\(s, x) -> maybe (error s) id x) $ xs

run :: PGF -> [GFact] -> IO ()
run g x = do
  case expand x of
    Left a -> mapM_ putStrLn (yell g a)
    Right (a, b) -> do
      putStrLn "* Cores:\n"
      yellSet g a
      putStrLn "* Facts:\n"
      yellSet g b
      putStrLn "* Explore:\n"
      mapM_ (\x -> putStrLn "* Option\n" >> yellSet g x) (explore b a)

      -- putStrLn ""
      -- yellSet g (wishes g a GTerapija)

-- wishes :: PGF -> Set GFact -> GSpot -> Set GWish
-- wishes _ xs spot =
--   flip foldMap xs $
--     \case
--       GYIsDoorFromX dst how src
--         | spot == dst
--           -> singleton (GWalk how src dst)
--       _ -> mempty

yellSet :: (Foldable t, Gf a, Show a) => PGF -> t a -> IO ()
yellSet g xs =
  mapM_ ((>> putStrLn "") . mapM_ putStrLn) (map (yell g) (toList xs))

readGrammar :: IO PGF
readGrammar = PGF.readPGF "Riga.pgf"

#if __GHCJS__

main :: IO ()
main = do
  callback <- asyncCallback1 $ \jss -> do
    putStrLn "Haskell: decoding Base64..."
    let bs = BS64.decodeLenient (Text.encodeUtf8 (JS.textFromJSVal jss))
    let pgf = PGF.Internal.decode (BS.fromStrict bs)
    putStrLn $ "Haskell: PGF has languages " ++ show (PGF.languages pgf)

  putStrLn "Haskell: calling JavaScript to fetch PGF..."
  fetchBase64 "lastadija.pgf" callback

#else

main :: IO ()
main = do
  g <- readGrammar
  lastadijaSpots g >>= run g

#endif
