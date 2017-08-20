{-# Language CPP #-}
{-# Language TupleSections #-}
{-# Language OverloadedStrings #-}

#ifdef __GHCJS__
{-# Language JavaScriptFFI #-}
#endif

module Kosmos
  ( main
  , PGF.linearizeAll
  , fetchGrammar
  , explore
  , relevantFacts
  , capitalize
  ) where

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
import Data.Text (Text)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Base64 as BS64

foreign import javascript interruptible
  "fetchBase64($1, $c);"
  fetchBase64 :: JS.JSString -> IO JSVal
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
explore :: [GFact] -> [GCore] -> [(GCore, [GFact])]
explore premises = mapMaybe (\x -> fmap (x,) (apply premises x))

core :: (GCore -> GCore) -> GCore
core = ($ GTrivial)

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
      mapM_ (\(x, _) -> putStrLn "* Option\n" >> mapM_ putStrLn (yell g x)) (explore b a)

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

main :: PGF -> IO (Either GFail ([GCore], [GFact]))
main pgf = do
  putStrLn "Haskell: calling JavaScript to fetch PGF..."
  bs2 <- fetchBytes "lastadija.txt"
  putStrLn $ "Haskell: PGF has languages " ++ show (PGF.languages pgf)
  let txt = Text.unpack (Text.decodeUtf8 bs2)
  return (expand (example pgf (lines txt)))

#else

main :: IO ()
main = do
  g <- readGrammar
  lastadijaSpots g >>= run g

#endif

capitalize = capitInit
