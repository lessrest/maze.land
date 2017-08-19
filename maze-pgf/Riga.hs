module Riga where

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

data GCore =
   GDoing GDeed GCore 
 | GGiving GFact GCore 
 | GKeeping GFact GCore 
 | GTaking GFact GCore 
 | GTrivial 
  deriving (Show, Eq, Ord)

data GDeed =
   GConsumption GItem 
 | GPresumption GItem 
  deriving (Show, Eq, Ord)

data GDig =
   GD_0 
 | GD_1 
 | GD_2 
 | GD_3 
 | GD_4 
 | GD_5 
 | GD_6 
 | GD_7 
 | GD_8 
 | GD_9 
  deriving (Show, Eq, Ord)

data GDigit =
   Gn2 
 | Gn3 
 | Gn4 
 | Gn5 
 | Gn6 
 | Gn7 
 | Gn8 
 | Gn9 
  deriving (Show, Eq, Ord)

data GDigits =
   GIDig GDig 
 | GIIDig GDig GDigits 
  deriving (Show, Eq, Ord)

data GDoor =
   GEast 
 | GNorth 
 | GNorthEast 
 | GNorthWest 
 | GSouth 
 | GSouthEast 
 | GSouthWest 
 | GWest 
  deriving (Show, Eq, Ord)

data GFact =
   GRuleApplies GRule 
 | GSpotHasDoor GSpot GDoor GSpot 
 | GSpotHasItem GSpot GItem 
 | GYouHaveItem GItem 
  deriving (Show, Eq, Ord)

data GFail = GDoorConflict GSpot GSpot GDoor GDoor 
  deriving (Show, Eq, Ord)

data GItem =
   GBoth GItem GItem 
 | GCount GSome GKind 
 | GPlayer 
  deriving (Show, Eq, Ord)

data GKind =
   GBike 
 | GCat 
 | GDog 
 | GEuro 
 | GPropKind GProp GKind 
 | GWatermelon 
  deriving (Show, Eq, Ord)

data GLine =
   GFactLine GFact 
 | GRuleLine GRule 
 | GYouSee GItem 
  deriving (Show, Eq, Ord)

data GNumeral = Gnum GSub1000000 
  deriving (Show, Eq, Ord)

data GProp =
   GLarge 
 | GSmall 
  deriving (Show, Eq, Ord)

data GRule =
   GCoreRule2 GDeed GFact 
 | GWhileRule GFact GDeed GItem 
  deriving (Show, Eq, Ord)

data GSome =
   GSome1 
 | GSome2 
 | GSome3 
 | GSome4 
 | GSomeNumber GNumeral 
  deriving (Show, Eq, Ord)

data GSpot =
   GMarket 
 | GSynagogue 
 | GT17 
 | GTerapija 
 | GUniversity 
 | Gspot_Agroprojekts 
 | Gspot_Banuzis 
 | Gspot_CafeRosemary 
 | Gspot_DagdasStreet 
 | Gspot_Deficits 
 | Gspot_GoodwillStudio 
 | Gspot_Idioma 
 | Gspot_Latgalite 
 | Gspot_MazaKrastaStreet 
 | Gspot_Pushkin11 
 | Gspot_SpekaStreet 
 | Gspot_Spikeri 
 | Gspot_SviestaPika 
 | Gspot_TheAbrenesStreetBusStation 
 | Gspot_TheBookShopsJanusAndGora 
 | Gspot_TheBusStation 
 | Gspot_TheCentralMarketPavilions 
 | Gspot_TheCentralMarketShoemaker 
 | Gspot_TheDaugavaSwimmingSpot 
 | Gspot_TheFreeRigaNeighborhoodResidence 
 | Gspot_TheGhettoMuseum 
 | Gspot_TheGreenField 
 | Gspot_TheHummusTeam 
 | Gspot_TheIndustrialGoodsMarket 
 | Gspot_TheJesusChurch 
 | Gspot_TheNightMarket 
 | Gspot_TheOrthodoxChurch 
 | Gspot_ThePan 
 | Gspot_ThePushkinStreetPrintShop 
 | Gspot_TheRedCrossSocialCenter 
 | Gspot_TheRiversideGallery 
 | Gspot_TheScienceAcademy 
 | Gspot_TheSoyShop 
 | Gspot_TheSpikeriPromenade 
 | Gspot_TheSynagogueMemorial 
 | Gspot_TheTunnel 
 | Gspot_TheVeraMuhinasMemorialHouse 
 | Gspot_VingrumaClub 
  deriving (Show, Eq, Ord)

data GSub10 =
   Gpot0 GDigit 
 | Gpot01 
  deriving (Show, Eq, Ord)

data GSub100 =
   Gpot0as1 GSub10 
 | Gpot1 GDigit 
 | Gpot110 
 | Gpot111 
 | Gpot1plus GDigit GSub10 
 | Gpot1to19 GDigit 
  deriving (Show, Eq, Ord)

data GSub1000 =
   Gpot1as2 GSub100 
 | Gpot2 GSub10 
 | Gpot2plus GSub10 GSub100 
  deriving (Show, Eq, Ord)

data GSub1000000 =
   Gpot2as3 GSub1000 
 | Gpot3 GSub1000 
 | Gpot3plus GSub1000 GSub1000 
  deriving (Show, Eq, Ord)


instance Gf GCore where
  gf (GDoing x1 x2) = mkApp (mkCId "Doing") [gf x1, gf x2]
  gf (GGiving x1 x2) = mkApp (mkCId "Giving") [gf x1, gf x2]
  gf (GKeeping x1 x2) = mkApp (mkCId "Keeping") [gf x1, gf x2]
  gf (GTaking x1 x2) = mkApp (mkCId "Taking") [gf x1, gf x2]
  gf GTrivial = mkApp (mkCId "Trivial") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "Doing" -> GDoing (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Giving" -> GGiving (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Keeping" -> GKeeping (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Taking" -> GTaking (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Trivial" -> GTrivial 


      _ -> error ("no Core " ++ show t)

instance Gf GDeed where
  gf (GConsumption x1) = mkApp (mkCId "Consumption") [gf x1]
  gf (GPresumption x1) = mkApp (mkCId "Presumption") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "Consumption" -> GConsumption (fg x1)
      Just (i,[x1]) | i == mkCId "Presumption" -> GPresumption (fg x1)


      _ -> error ("no Deed " ++ show t)

instance Gf GDig where
  gf GD_0 = mkApp (mkCId "D_0") []
  gf GD_1 = mkApp (mkCId "D_1") []
  gf GD_2 = mkApp (mkCId "D_2") []
  gf GD_3 = mkApp (mkCId "D_3") []
  gf GD_4 = mkApp (mkCId "D_4") []
  gf GD_5 = mkApp (mkCId "D_5") []
  gf GD_6 = mkApp (mkCId "D_6") []
  gf GD_7 = mkApp (mkCId "D_7") []
  gf GD_8 = mkApp (mkCId "D_8") []
  gf GD_9 = mkApp (mkCId "D_9") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "D_0" -> GD_0 
      Just (i,[]) | i == mkCId "D_1" -> GD_1 
      Just (i,[]) | i == mkCId "D_2" -> GD_2 
      Just (i,[]) | i == mkCId "D_3" -> GD_3 
      Just (i,[]) | i == mkCId "D_4" -> GD_4 
      Just (i,[]) | i == mkCId "D_5" -> GD_5 
      Just (i,[]) | i == mkCId "D_6" -> GD_6 
      Just (i,[]) | i == mkCId "D_7" -> GD_7 
      Just (i,[]) | i == mkCId "D_8" -> GD_8 
      Just (i,[]) | i == mkCId "D_9" -> GD_9 


      _ -> error ("no Dig " ++ show t)

instance Gf GDigit where
  gf Gn2 = mkApp (mkCId "n2") []
  gf Gn3 = mkApp (mkCId "n3") []
  gf Gn4 = mkApp (mkCId "n4") []
  gf Gn5 = mkApp (mkCId "n5") []
  gf Gn6 = mkApp (mkCId "n6") []
  gf Gn7 = mkApp (mkCId "n7") []
  gf Gn8 = mkApp (mkCId "n8") []
  gf Gn9 = mkApp (mkCId "n9") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "n2" -> Gn2 
      Just (i,[]) | i == mkCId "n3" -> Gn3 
      Just (i,[]) | i == mkCId "n4" -> Gn4 
      Just (i,[]) | i == mkCId "n5" -> Gn5 
      Just (i,[]) | i == mkCId "n6" -> Gn6 
      Just (i,[]) | i == mkCId "n7" -> Gn7 
      Just (i,[]) | i == mkCId "n8" -> Gn8 
      Just (i,[]) | i == mkCId "n9" -> Gn9 


      _ -> error ("no Digit " ++ show t)

instance Gf GDigits where
  gf (GIDig x1) = mkApp (mkCId "IDig") [gf x1]
  gf (GIIDig x1 x2) = mkApp (mkCId "IIDig") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "IDig" -> GIDig (fg x1)
      Just (i,[x1,x2]) | i == mkCId "IIDig" -> GIIDig (fg x1) (fg x2)


      _ -> error ("no Digits " ++ show t)

instance Gf GDoor where
  gf GEast = mkApp (mkCId "East") []
  gf GNorth = mkApp (mkCId "North") []
  gf GNorthEast = mkApp (mkCId "NorthEast") []
  gf GNorthWest = mkApp (mkCId "NorthWest") []
  gf GSouth = mkApp (mkCId "South") []
  gf GSouthEast = mkApp (mkCId "SouthEast") []
  gf GSouthWest = mkApp (mkCId "SouthWest") []
  gf GWest = mkApp (mkCId "West") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "East" -> GEast 
      Just (i,[]) | i == mkCId "North" -> GNorth 
      Just (i,[]) | i == mkCId "NorthEast" -> GNorthEast 
      Just (i,[]) | i == mkCId "NorthWest" -> GNorthWest 
      Just (i,[]) | i == mkCId "South" -> GSouth 
      Just (i,[]) | i == mkCId "SouthEast" -> GSouthEast 
      Just (i,[]) | i == mkCId "SouthWest" -> GSouthWest 
      Just (i,[]) | i == mkCId "West" -> GWest 


      _ -> error ("no Door " ++ show t)

instance Gf GFact where
  gf (GRuleApplies x1) = mkApp (mkCId "RuleApplies") [gf x1]
  gf (GSpotHasDoor x1 x2 x3) = mkApp (mkCId "SpotHasDoor") [gf x1, gf x2, gf x3]
  gf (GSpotHasItem x1 x2) = mkApp (mkCId "SpotHasItem") [gf x1, gf x2]
  gf (GYouHaveItem x1) = mkApp (mkCId "YouHaveItem") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "RuleApplies" -> GRuleApplies (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "SpotHasDoor" -> GSpotHasDoor (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "SpotHasItem" -> GSpotHasItem (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "YouHaveItem" -> GYouHaveItem (fg x1)


      _ -> error ("no Fact " ++ show t)

instance Gf GFail where
  gf (GDoorConflict x1 x2 x3 x4) = mkApp (mkCId "DoorConflict") [gf x1, gf x2, gf x3, gf x4]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3,x4]) | i == mkCId "DoorConflict" -> GDoorConflict (fg x1) (fg x2) (fg x3) (fg x4)


      _ -> error ("no Fail " ++ show t)

instance Gf GItem where
  gf (GBoth x1 x2) = mkApp (mkCId "Both") [gf x1, gf x2]
  gf (GCount x1 x2) = mkApp (mkCId "Count") [gf x1, gf x2]
  gf GPlayer = mkApp (mkCId "Player") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "Both" -> GBoth (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Count" -> GCount (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Player" -> GPlayer 


      _ -> error ("no Item " ++ show t)

instance Gf GKind where
  gf GBike = mkApp (mkCId "Bike") []
  gf GCat = mkApp (mkCId "Cat") []
  gf GDog = mkApp (mkCId "Dog") []
  gf GEuro = mkApp (mkCId "Euro") []
  gf (GPropKind x1 x2) = mkApp (mkCId "PropKind") [gf x1, gf x2]
  gf GWatermelon = mkApp (mkCId "Watermelon") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Bike" -> GBike 
      Just (i,[]) | i == mkCId "Cat" -> GCat 
      Just (i,[]) | i == mkCId "Dog" -> GDog 
      Just (i,[]) | i == mkCId "Euro" -> GEuro 
      Just (i,[x1,x2]) | i == mkCId "PropKind" -> GPropKind (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Watermelon" -> GWatermelon 


      _ -> error ("no Kind " ++ show t)

instance Gf GLine where
  gf (GFactLine x1) = mkApp (mkCId "FactLine") [gf x1]
  gf (GRuleLine x1) = mkApp (mkCId "RuleLine") [gf x1]
  gf (GYouSee x1) = mkApp (mkCId "YouSee") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "FactLine" -> GFactLine (fg x1)
      Just (i,[x1]) | i == mkCId "RuleLine" -> GRuleLine (fg x1)
      Just (i,[x1]) | i == mkCId "YouSee" -> GYouSee (fg x1)


      _ -> error ("no Line " ++ show t)

instance Gf GNumeral where
  gf (Gnum x1) = mkApp (mkCId "num") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "num" -> Gnum (fg x1)


      _ -> error ("no Numeral " ++ show t)

instance Gf GProp where
  gf GLarge = mkApp (mkCId "Large") []
  gf GSmall = mkApp (mkCId "Small") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Large" -> GLarge 
      Just (i,[]) | i == mkCId "Small" -> GSmall 


      _ -> error ("no Prop " ++ show t)

instance Gf GRule where
  gf (GCoreRule2 x1 x2) = mkApp (mkCId "CoreRule2") [gf x1, gf x2]
  gf (GWhileRule x1 x2 x3) = mkApp (mkCId "WhileRule") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "CoreRule2" -> GCoreRule2 (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "WhileRule" -> GWhileRule (fg x1) (fg x2) (fg x3)


      _ -> error ("no Rule " ++ show t)

instance Gf GSome where
  gf GSome1 = mkApp (mkCId "Some1") []
  gf GSome2 = mkApp (mkCId "Some2") []
  gf GSome3 = mkApp (mkCId "Some3") []
  gf GSome4 = mkApp (mkCId "Some4") []
  gf (GSomeNumber x1) = mkApp (mkCId "SomeNumber") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Some1" -> GSome1 
      Just (i,[]) | i == mkCId "Some2" -> GSome2 
      Just (i,[]) | i == mkCId "Some3" -> GSome3 
      Just (i,[]) | i == mkCId "Some4" -> GSome4 
      Just (i,[x1]) | i == mkCId "SomeNumber" -> GSomeNumber (fg x1)


      _ -> error ("no Some " ++ show t)

instance Gf GSpot where
  gf GMarket = mkApp (mkCId "Market") []
  gf GSynagogue = mkApp (mkCId "Synagogue") []
  gf GT17 = mkApp (mkCId "T17") []
  gf GTerapija = mkApp (mkCId "Terapija") []
  gf GUniversity = mkApp (mkCId "University") []
  gf Gspot_Agroprojekts = mkApp (mkCId "spot_Agroprojekts") []
  gf Gspot_Banuzis = mkApp (mkCId "spot_Banuzis") []
  gf Gspot_CafeRosemary = mkApp (mkCId "spot_CafeRosemary") []
  gf Gspot_DagdasStreet = mkApp (mkCId "spot_DagdasStreet") []
  gf Gspot_Deficits = mkApp (mkCId "spot_Deficits") []
  gf Gspot_GoodwillStudio = mkApp (mkCId "spot_GoodwillStudio") []
  gf Gspot_Idioma = mkApp (mkCId "spot_Idioma") []
  gf Gspot_Latgalite = mkApp (mkCId "spot_Latgalite") []
  gf Gspot_MazaKrastaStreet = mkApp (mkCId "spot_MazaKrastaStreet") []
  gf Gspot_Pushkin11 = mkApp (mkCId "spot_Pushkin11") []
  gf Gspot_SpekaStreet = mkApp (mkCId "spot_SpekaStreet") []
  gf Gspot_Spikeri = mkApp (mkCId "spot_Spikeri") []
  gf Gspot_SviestaPika = mkApp (mkCId "spot_SviestaPika") []
  gf Gspot_TheAbrenesStreetBusStation = mkApp (mkCId "spot_TheAbrenesStreetBusStation") []
  gf Gspot_TheBookShopsJanusAndGora = mkApp (mkCId "spot_TheBookShopsJanusAndGora") []
  gf Gspot_TheBusStation = mkApp (mkCId "spot_TheBusStation") []
  gf Gspot_TheCentralMarketPavilions = mkApp (mkCId "spot_TheCentralMarketPavilions") []
  gf Gspot_TheCentralMarketShoemaker = mkApp (mkCId "spot_TheCentralMarketShoemaker") []
  gf Gspot_TheDaugavaSwimmingSpot = mkApp (mkCId "spot_TheDaugavaSwimmingSpot") []
  gf Gspot_TheFreeRigaNeighborhoodResidence = mkApp (mkCId "spot_TheFreeRigaNeighborhoodResidence") []
  gf Gspot_TheGhettoMuseum = mkApp (mkCId "spot_TheGhettoMuseum") []
  gf Gspot_TheGreenField = mkApp (mkCId "spot_TheGreenField") []
  gf Gspot_TheHummusTeam = mkApp (mkCId "spot_TheHummusTeam") []
  gf Gspot_TheIndustrialGoodsMarket = mkApp (mkCId "spot_TheIndustrialGoodsMarket") []
  gf Gspot_TheJesusChurch = mkApp (mkCId "spot_TheJesusChurch") []
  gf Gspot_TheNightMarket = mkApp (mkCId "spot_TheNightMarket") []
  gf Gspot_TheOrthodoxChurch = mkApp (mkCId "spot_TheOrthodoxChurch") []
  gf Gspot_ThePan = mkApp (mkCId "spot_ThePan") []
  gf Gspot_ThePushkinStreetPrintShop = mkApp (mkCId "spot_ThePushkinStreetPrintShop") []
  gf Gspot_TheRedCrossSocialCenter = mkApp (mkCId "spot_TheRedCrossSocialCenter") []
  gf Gspot_TheRiversideGallery = mkApp (mkCId "spot_TheRiversideGallery") []
  gf Gspot_TheScienceAcademy = mkApp (mkCId "spot_TheScienceAcademy") []
  gf Gspot_TheSoyShop = mkApp (mkCId "spot_TheSoyShop") []
  gf Gspot_TheSpikeriPromenade = mkApp (mkCId "spot_TheSpikeriPromenade") []
  gf Gspot_TheSynagogueMemorial = mkApp (mkCId "spot_TheSynagogueMemorial") []
  gf Gspot_TheTunnel = mkApp (mkCId "spot_TheTunnel") []
  gf Gspot_TheVeraMuhinasMemorialHouse = mkApp (mkCId "spot_TheVeraMuhinasMemorialHouse") []
  gf Gspot_VingrumaClub = mkApp (mkCId "spot_VingrumaClub") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Market" -> GMarket 
      Just (i,[]) | i == mkCId "Synagogue" -> GSynagogue 
      Just (i,[]) | i == mkCId "T17" -> GT17 
      Just (i,[]) | i == mkCId "Terapija" -> GTerapija 
      Just (i,[]) | i == mkCId "University" -> GUniversity 
      Just (i,[]) | i == mkCId "spot_Agroprojekts" -> Gspot_Agroprojekts 
      Just (i,[]) | i == mkCId "spot_Banuzis" -> Gspot_Banuzis 
      Just (i,[]) | i == mkCId "spot_CafeRosemary" -> Gspot_CafeRosemary 
      Just (i,[]) | i == mkCId "spot_DagdasStreet" -> Gspot_DagdasStreet 
      Just (i,[]) | i == mkCId "spot_Deficits" -> Gspot_Deficits 
      Just (i,[]) | i == mkCId "spot_GoodwillStudio" -> Gspot_GoodwillStudio 
      Just (i,[]) | i == mkCId "spot_Idioma" -> Gspot_Idioma 
      Just (i,[]) | i == mkCId "spot_Latgalite" -> Gspot_Latgalite 
      Just (i,[]) | i == mkCId "spot_MazaKrastaStreet" -> Gspot_MazaKrastaStreet 
      Just (i,[]) | i == mkCId "spot_Pushkin11" -> Gspot_Pushkin11 
      Just (i,[]) | i == mkCId "spot_SpekaStreet" -> Gspot_SpekaStreet 
      Just (i,[]) | i == mkCId "spot_Spikeri" -> Gspot_Spikeri 
      Just (i,[]) | i == mkCId "spot_SviestaPika" -> Gspot_SviestaPika 
      Just (i,[]) | i == mkCId "spot_TheAbrenesStreetBusStation" -> Gspot_TheAbrenesStreetBusStation 
      Just (i,[]) | i == mkCId "spot_TheBookShopsJanusAndGora" -> Gspot_TheBookShopsJanusAndGora 
      Just (i,[]) | i == mkCId "spot_TheBusStation" -> Gspot_TheBusStation 
      Just (i,[]) | i == mkCId "spot_TheCentralMarketPavilions" -> Gspot_TheCentralMarketPavilions 
      Just (i,[]) | i == mkCId "spot_TheCentralMarketShoemaker" -> Gspot_TheCentralMarketShoemaker 
      Just (i,[]) | i == mkCId "spot_TheDaugavaSwimmingSpot" -> Gspot_TheDaugavaSwimmingSpot 
      Just (i,[]) | i == mkCId "spot_TheFreeRigaNeighborhoodResidence" -> Gspot_TheFreeRigaNeighborhoodResidence 
      Just (i,[]) | i == mkCId "spot_TheGhettoMuseum" -> Gspot_TheGhettoMuseum 
      Just (i,[]) | i == mkCId "spot_TheGreenField" -> Gspot_TheGreenField 
      Just (i,[]) | i == mkCId "spot_TheHummusTeam" -> Gspot_TheHummusTeam 
      Just (i,[]) | i == mkCId "spot_TheIndustrialGoodsMarket" -> Gspot_TheIndustrialGoodsMarket 
      Just (i,[]) | i == mkCId "spot_TheJesusChurch" -> Gspot_TheJesusChurch 
      Just (i,[]) | i == mkCId "spot_TheNightMarket" -> Gspot_TheNightMarket 
      Just (i,[]) | i == mkCId "spot_TheOrthodoxChurch" -> Gspot_TheOrthodoxChurch 
      Just (i,[]) | i == mkCId "spot_ThePan" -> Gspot_ThePan 
      Just (i,[]) | i == mkCId "spot_ThePushkinStreetPrintShop" -> Gspot_ThePushkinStreetPrintShop 
      Just (i,[]) | i == mkCId "spot_TheRedCrossSocialCenter" -> Gspot_TheRedCrossSocialCenter 
      Just (i,[]) | i == mkCId "spot_TheRiversideGallery" -> Gspot_TheRiversideGallery 
      Just (i,[]) | i == mkCId "spot_TheScienceAcademy" -> Gspot_TheScienceAcademy 
      Just (i,[]) | i == mkCId "spot_TheSoyShop" -> Gspot_TheSoyShop 
      Just (i,[]) | i == mkCId "spot_TheSpikeriPromenade" -> Gspot_TheSpikeriPromenade 
      Just (i,[]) | i == mkCId "spot_TheSynagogueMemorial" -> Gspot_TheSynagogueMemorial 
      Just (i,[]) | i == mkCId "spot_TheTunnel" -> Gspot_TheTunnel 
      Just (i,[]) | i == mkCId "spot_TheVeraMuhinasMemorialHouse" -> Gspot_TheVeraMuhinasMemorialHouse 
      Just (i,[]) | i == mkCId "spot_VingrumaClub" -> Gspot_VingrumaClub 


      _ -> error ("no Spot " ++ show t)

instance Gf GSub10 where
  gf (Gpot0 x1) = mkApp (mkCId "pot0") [gf x1]
  gf Gpot01 = mkApp (mkCId "pot01") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot0" -> Gpot0 (fg x1)
      Just (i,[]) | i == mkCId "pot01" -> Gpot01 


      _ -> error ("no Sub10 " ++ show t)

instance Gf GSub100 where
  gf (Gpot0as1 x1) = mkApp (mkCId "pot0as1") [gf x1]
  gf (Gpot1 x1) = mkApp (mkCId "pot1") [gf x1]
  gf Gpot110 = mkApp (mkCId "pot110") []
  gf Gpot111 = mkApp (mkCId "pot111") []
  gf (Gpot1plus x1 x2) = mkApp (mkCId "pot1plus") [gf x1, gf x2]
  gf (Gpot1to19 x1) = mkApp (mkCId "pot1to19") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot0as1" -> Gpot0as1 (fg x1)
      Just (i,[x1]) | i == mkCId "pot1" -> Gpot1 (fg x1)
      Just (i,[]) | i == mkCId "pot110" -> Gpot110 
      Just (i,[]) | i == mkCId "pot111" -> Gpot111 
      Just (i,[x1,x2]) | i == mkCId "pot1plus" -> Gpot1plus (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "pot1to19" -> Gpot1to19 (fg x1)


      _ -> error ("no Sub100 " ++ show t)

instance Gf GSub1000 where
  gf (Gpot1as2 x1) = mkApp (mkCId "pot1as2") [gf x1]
  gf (Gpot2 x1) = mkApp (mkCId "pot2") [gf x1]
  gf (Gpot2plus x1 x2) = mkApp (mkCId "pot2plus") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot1as2" -> Gpot1as2 (fg x1)
      Just (i,[x1]) | i == mkCId "pot2" -> Gpot2 (fg x1)
      Just (i,[x1,x2]) | i == mkCId "pot2plus" -> Gpot2plus (fg x1) (fg x2)


      _ -> error ("no Sub1000 " ++ show t)

instance Gf GSub1000000 where
  gf (Gpot2as3 x1) = mkApp (mkCId "pot2as3") [gf x1]
  gf (Gpot3 x1) = mkApp (mkCId "pot3") [gf x1]
  gf (Gpot3plus x1 x2) = mkApp (mkCId "pot3plus") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "pot2as3" -> Gpot2as3 (fg x1)
      Just (i,[x1]) | i == mkCId "pot3" -> Gpot3 (fg x1)
      Just (i,[x1,x2]) | i == mkCId "pot3plus" -> Gpot3plus (fg x1) (fg x2)


      _ -> error ("no Sub1000000 " ++ show t)


