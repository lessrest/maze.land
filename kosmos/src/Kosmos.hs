module Kosmos where

import Riga
import PGF (PGF)
import qualified PGF as GF

--import Data.Set (Set, singleton, fromList)
import Data.Foldable (Foldable, foldMap, toList)
import Control.Monad (msum)

import qualified Data.List as List

data Item
  = One GKind
  deriving (Eq, Ord, Show)

data Fact
  = Placement GSpot Item
  | Residence GSpot
  | Possession Item
  | Ruling Rule
  deriving (Eq, Ord, Show)

data Need
  = Consumption Fact
  | Presumption Fact
  deriving (Eq, Ord, Show)

data Deed
  = Produce Fact
  | Consume Fact
  deriving (Eq, Ord, Show)

data Rule
  = Rule [Need] [Fact]
  deriving (Eq, Ord, Show)

attempt :: [Fact] -> Rule -> Either Need [Fact]
attempt facts (Rule needs news) =
  case needs of
    [] -> Right (facts ++ news)
    (x:xs) ->
      case x of
        Presumption fact ->
          if fact `elem` facts
          then attempt facts (Rule xs news)
          else Left x
        Consumption fact ->
          if fact `elem` facts
          then attempt (List.delete fact facts) (Rule xs news)
          else Left x

grok :: GFact -> [Fact]
grok = \case
  GSpotHasItem gSpot gItem ->
    map (Placement gSpot) (grokItem gItem)

  where
    grokItem = \case
      GBoth a b -> grokItem a ++ grokItem b
      GCount GSome1 x -> [One x]
      GCount GSome2 x -> [One x, One x]
      GCount GSome3 x -> [One x, One x, One x]
      GCount GSome4 x -> [One x, One x, One x, One x]

readGrammar :: IO PGF
readGrammar = GF.readPGF "Riga.pgf"

parseFact :: PGF -> String -> Maybe GFact
parseFact g s =
  let parses = GF.parseAllLang g (GF.startCat g) s
  in case (do (_, trees) <- parses
              map (fg :: GF.Tree -> GLine) trees) of
    [GFactLine a] -> Just a
    [GRuleLine a] -> Just (GRuleApplies a)
    _   -> Nothing

expand :: [GFact] -> Either GFail ([GCore], [GFact])
expand = fmap (foldMap stage2) . check . foldMap stage1
  where
    stage1 = \case
      -- Reverse doors
      GSpotHasDoor src how dst ->
        [ GSpotHasDoor src how dst
        , GSpotHasDoor dst (back how) src ]
      x ->
        [x]

    stage2 = \case
      -- Make doors into rules
      GSpotHasDoor src how dst ->
        ([ GTaking (GSpotHasItem src GPlayer)
            (GGiving (GSpotHasItem dst GPlayer)
              (GKeeping (GSpotHasDoor src how dst)
                GTrivial))
         ], [GSpotHasDoor src how dst])

      -- Expand `while' rules to core rules
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

      x -> ([], [x])

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
              _ -> Nothing) xs
    bad _ = Nothing

yell :: (Show a, Gf a) => PGF -> a -> [String]
yell g x =
  show x : GF.linearizeAll g (gf x)

back :: GDoor -> GDoor
back = \case
  GNorth -> GSouth
  GSouth -> GNorth
  GWest -> GEast
  GEast -> GWest

example1 :: PGF -> [GFact]
example1 g = example g
  [ "Terapija is north from the central market"
  , "four big watermelons are in the central market"
  , "rule: when the player is in the central market "
      ++ "if you spend one euro then you get one watermelon"
  , "the player is in Terapija"
  , "you have three euros"
  ]

example :: PGF -> [String] -> [GFact]
example g ss =
  let xs = map (\x -> (x, parseFact g x)) ss
  in map (\(s, x) -> maybe (error s) id x) $ xs

run :: PGF -> [GFact] -> IO ()
run g x = do
  case expand x of
    Left a -> mapM_ putStrLn (yell g a)
    Right (a, b) -> do
      yellSet g a
      yellSet g b
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

main :: IO ()
main = do
  g <- readGrammar
  run g (example1 g)
