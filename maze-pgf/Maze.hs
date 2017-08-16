module Maze where

import PGF hiding (Tree)
import qualified PGF
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> PGF.Tree
  fg :: PGF.Tree -> a

newtype GString = GString String  deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int  deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double  deriving Show

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
  deriving Show

data GFact =
   GAnXIsAtY GItem GSpot 
 | GYIsDoorFromX GSpot GDoor GSpot 
  deriving Show

data GItem =
   GCat 
 | GDog 
 | GPropItem GProp GItem 
  deriving Show

data GLine = GFactLine GFact 
  deriving Show

data GProp =
   GLarge 
 | GSmall 
  deriving Show

data GSpot =
   GMarket 
 | GSynagogue 
 | GT17 
 | GTerapija 
 | GUniversity 
  deriving Show


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

instance Gf GItem where
  gf GCat = mkApp (mkCId "Cat") []
  gf GDog = mkApp (mkCId "Dog") []
  gf (GPropItem x1 x2) = mkApp (mkCId "PropItem") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Cat" -> GCat 
      Just (i,[]) | i == mkCId "Dog" -> GDog 
      Just (i,[x1,x2]) | i == mkCId "PropItem" -> GPropItem (fg x1) (fg x2)


      _ -> error ("no Item " ++ show t)

instance Gf GLine where
  gf (GFactLine x1) = mkApp (mkCId "FactLine") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "FactLine" -> GFactLine (fg x1)


      _ -> error ("no Line " ++ show t)

instance Gf GProp where
  gf GLarge = mkApp (mkCId "Large") []
  gf GSmall = mkApp (mkCId "Small") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Large" -> GLarge 
      Just (i,[]) | i == mkCId "Small" -> GSmall 


      _ -> error ("no Prop " ++ show t)

instance Gf GSpot where
  gf GMarket = mkApp (mkCId "Market") []
  gf GSynagogue = mkApp (mkCId "Synagogue") []
  gf GT17 = mkApp (mkCId "T17") []
  gf GTerapija = mkApp (mkCId "Terapija") []
  gf GUniversity = mkApp (mkCId "University") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Market" -> GMarket 
      Just (i,[]) | i == mkCId "Synagogue" -> GSynagogue 
      Just (i,[]) | i == mkCId "T17" -> GT17 
      Just (i,[]) | i == mkCId "Terapija" -> GTerapija 
      Just (i,[]) | i == mkCId "University" -> GUniversity 


      _ -> error ("no Spot " ++ show t)


