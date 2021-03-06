module Maze where

import PGF hiding (Tree)
import qualified PGF
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> PGF.Tree
  fg :: PGF.Tree -> a

newtype GString = GString String  deriving (Show, Eq, Ord)

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int  deriving (Show, Eq, Ord)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double  deriving (Show, Eq, Ord)

instance Gf GFloat where
  gf (GFloat x) = mkDouble x
  fg t =
    case unDouble t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GDoor =
   GEast 
 | GNorth 
 | GSouth 
 | GWest 
  deriving (Show, Eq, Ord)

data GFact =
   GAnXIsAtY GItem GSpot 
 | GYIsDoorFromX GSpot GDoor GSpot 
  deriving (Show, Eq, Ord)

data GFail = GDoorConflict GSpot GSpot GDoor GDoor 
  deriving (Show, Eq, Ord)

data GItem =
   GMany GKind 
 | GOne GKind 
  deriving (Show, Eq, Ord)

data GKind =
   GBike 
 | GCat 
 | GDog 
 | GPropKind GProp GKind 
 | GWatermelon 
  deriving (Show, Eq, Ord)

data GLine =
   GFactLine GFact 
 | GWishLine GWish 
 | GYouSeeX GItem 
  deriving (Show, Eq, Ord)

data GProp =
   GLarge 
 | GSmall 
  deriving (Show, Eq, Ord)

data GWish = GWalk GDoor GSpot GSpot 
  deriving (Show, Eq, Ord)

data GSpot


instance Gf GDoor where
  gf GEast = mkApp (mkCId "East") []
  gf GNorth = mkApp (mkCId "North") []
  gf GSouth = mkApp (mkCId "South") []
  gf GWest = mkApp (mkCId "West") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "East" -> GEast 
      Just (i,[]) | i == mkCId "North" -> GNorth 
      Just (i,[]) | i == mkCId "South" -> GSouth 
      Just (i,[]) | i == mkCId "West" -> GWest 


      _ -> error ("no Door " ++ show t)

instance Gf GFact where
  gf (GAnXIsAtY x1 x2) = mkApp (mkCId "AnXIsAtY") [gf x1, gf x2]
  gf (GYIsDoorFromX x1 x2 x3) = mkApp (mkCId "YIsDoorFromX") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AnXIsAtY" -> GAnXIsAtY (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "YIsDoorFromX" -> GYIsDoorFromX (fg x1) (fg x2) (fg x3)


      _ -> error ("no Fact " ++ show t)

instance Gf GFail where
  gf (GDoorConflict x1 x2 x3 x4) = mkApp (mkCId "DoorConflict") [gf x1, gf x2, gf x3, gf x4]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3,x4]) | i == mkCId "DoorConflict" -> GDoorConflict (fg x1) (fg x2) (fg x3) (fg x4)


      _ -> error ("no Fail " ++ show t)

instance Gf GItem where
  gf (GMany x1) = mkApp (mkCId "Many") [gf x1]
  gf (GOne x1) = mkApp (mkCId "One") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "Many" -> GMany (fg x1)
      Just (i,[x1]) | i == mkCId "One" -> GOne (fg x1)


      _ -> error ("no Item " ++ show t)

instance Gf GKind where
  gf GBike = mkApp (mkCId "Bike") []
  gf GCat = mkApp (mkCId "Cat") []
  gf GDog = mkApp (mkCId "Dog") []
  gf (GPropKind x1 x2) = mkApp (mkCId "PropKind") [gf x1, gf x2]
  gf GWatermelon = mkApp (mkCId "Watermelon") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Bike" -> GBike 
      Just (i,[]) | i == mkCId "Cat" -> GCat 
      Just (i,[]) | i == mkCId "Dog" -> GDog 
      Just (i,[x1,x2]) | i == mkCId "PropKind" -> GPropKind (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Watermelon" -> GWatermelon 


      _ -> error ("no Kind " ++ show t)

instance Gf GLine where
  gf (GFactLine x1) = mkApp (mkCId "FactLine") [gf x1]
  gf (GWishLine x1) = mkApp (mkCId "WishLine") [gf x1]
  gf (GYouSeeX x1) = mkApp (mkCId "YouSeeX") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "FactLine" -> GFactLine (fg x1)
      Just (i,[x1]) | i == mkCId "WishLine" -> GWishLine (fg x1)
      Just (i,[x1]) | i == mkCId "YouSeeX" -> GYouSeeX (fg x1)


      _ -> error ("no Line " ++ show t)

instance Gf GProp where
  gf GLarge = mkApp (mkCId "Large") []
  gf GSmall = mkApp (mkCId "Small") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Large" -> GLarge 
      Just (i,[]) | i == mkCId "Small" -> GSmall 


      _ -> error ("no Prop " ++ show t)

instance Gf GWish where
  gf (GWalk x1 x2 x3) = mkApp (mkCId "Walk") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "Walk" -> GWalk (fg x1) (fg x2) (fg x3)


      _ -> error ("no Wish " ++ show t)

instance Show GSpot

instance Gf GSpot where
  gf _ = undefined
  fg _ = undefined




