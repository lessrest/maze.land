module Kosmos where

import Maze
import PGF (PGF)
import qualified PGF as GF

import Data.Set (Set, singleton, fromList)
import Data.Foldable (foldMap, toList, find)
import Data.Maybe (fromJust)
import Control.Monad (msum)

readGrammar :: IO PGF
readGrammar = GF.readPGF "Maze.pgf"

parseLine :: PGF -> String -> Maybe GLine
parseLine g s =
  let parses = GF.parseAllLang g (GF.startCat g) s
  in case (do (_, trees) <- parses
              map (fg :: GF.Tree -> GLine) trees) of
    [a] -> Just a
    _   -> Nothing

expand :: Set GLine -> Either GFail (Set GLine)
expand xs = check (foldMap f xs)
  where
    f (x@(GFactLine (GYIsDoorFromX src how dst))) =
      fromList
        [ x, GFactLine (GYIsDoorFromX dst (back how) src) ]
    f x = singleton x

check :: Set GLine -> Either GFail (Set GLine)
check xs =
  case msum (map bad (toList xs)) of
    Nothing -> Right xs
    Just (src, how1, how2, dst) ->
      Left (GDoorConflict src dst how1 how2)
  where
    bad (GFactLine (GYIsDoorFromX src1 how1 dst1)) =
      msum $ map (\case
              GFactLine (GYIsDoorFromX src2 how2 dst2)
                | src1 == src2 && dst1 == dst2 && how1 /= how2
                  -> Just (src1, how1, how2, dst1)
              _ -> Nothing) (toList xs)
    bad _ = Nothing

yell :: Gf a => PGF -> a -> [String]
yell g x =
  GF.linearizeAll g (gf x)

back :: GDoor -> GDoor
back = \case
  GNorth -> GSouth
  GSouth -> GNorth
  GWest -> GEast
  GEast -> GWest

example1 :: PGF -> Set GLine
example1 g = example g
  [ "T17 is east of Terapija"
  , "Terapija is north of the central market"
  , "a small cat is in T17"
  ]

example :: PGF -> [String] -> Set GLine
example g s =
  let xs = map (parseLine g) s
  in fromList . map fromJust $ xs

run :: PGF -> Set GLine -> IO ()
run g x = do
  case expand x of
    Left a -> mapM_ putStrLn (yell g a)
    Right a -> mapM_ ((>> putStrLn "") . mapM_ putStrLn) (map (yell g) (toList a))

main :: IO ()
main = do
  g <- readGrammar
  run g (example1 g)
