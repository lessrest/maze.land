concrete RigaEng of Riga = MazeEng **
  open SyntaxEng, ParadigmsEng
in {
  flags coding = utf8;
  oper
    mkSpot : Str -> NP;
    mkSpot s = mkNP (mkPN s);

    mkTheSpots : Str -> NP;
    mkTheSpots s = mkNP thePl_Det (mkN s);

  lin
    spot_Agroprojekts = mkSpot "\"Agroprojekts\"";
    spot_Banuzis = mkSpot "Banuzis";
    spot_CafeRosemary = mkSpot "Café Rosemary";
    spot_DagdasStreet = mkSpot "Dagdas Street";
    spot_Deficits = mkSpot "Deficits";
    spot_GoodwillStudio = mkSpot "Goodwill Studio";
    spot_Idioma = mkSpot "\"Idioma\"";
    spot_Latgalite = mkSpot "Latgalite";
    spot_MazaKrastaStreet = mkSpot "Maza Krasta Street";
    spot_Pushkin11 = mkSpot "Pushkin 11";
    spot_SpekaStreet = mkSpot "Speka Street";
    spot_Spikeri = mkSpot "Spikeri";
    spot_SviestaPika = mkSpot "Sviesta pika";
    spot_TheAbrenesStreetBusStation = mkSpot "the Abrenes Street bus station";
    spot_TheBookShopsJanusAndGora = mkNP thePl_Det (mkCN (mkN "book shop") (mkNP and_Conj (mkNP (mkPN "\"Janus\"")) (mkNP (mkPN "\"Gora\""))));
    spot_TheBusStation = mkSpot "the bus station";
    spot_TheCentralMarketStands = mkTheSpots "central market stand";
    spot_TheCentralMarketShoemaker = mkSpot "the central market shoemaker";
    spot_TheDaugavaSwimmingSpot = mkSpot "the Daugava swimming spot";
    spot_TheFreeRigaNeighborhoodResidence = mkSpot "the Free Riga neighborhood residence";
    spot_TheGhettoMuseum = mkSpot "the ghetto museum";
    spot_TheGreenField = mkSpot "the green field";
    spot_TheHummusTeam = mkSpot "the Hummus Team";
    spot_TheIndustrialGoodsMarket = mkSpot "the industrial goods market";
    spot_TheJesusChurch = mkSpot "the Jesus Church";
    spot_TheNightMarket = mkSpot "the night market";
    spot_TheOrthodoxChurch = mkSpot "the Orthodox Church";
    spot_ThePan = mkSpot "\"the pan\"";
    spot_ThePushkinStreetPrintShop = mkSpot "the Pushkin Street print shop";
    spot_TheRedCrossSocialCenter = mkSpot "the Red Cross social center";
    spot_TheRiversideGallery = mkSpot "the Riverside Gallery";
    spot_TheScienceAcademy = mkSpot "the Science Academy";
    spot_TheSoyShop = mkSpot "the soy shop";
    spot_TheSpikeriPromenade = mkSpot "the Spikeri promenade";
    spot_TheSynagogueMemorial = mkSpot "the synagogue memorial";
    spot_TheTunnel = mkSpot "the tunnel";
    spot_TheVeraMuhinasMemorialHouse = mkSpot "the Vera Muhinas memorial house";
    spot_VingrumaClub = mkSpot "Vingruma Club";
    spot_TheMilkPavilion = mkSpot "the milk pavilion";
    spot_TheEmptyPavilion = mkSpot "the empty pavilion";
    spot_TheVegetablePavilion = mkSpot "the vegetable pavilion";
    spot_TheFishPavilion = mkSpot "the fish pavilion";
    spot_OutsideTheUzbekPlace = mkSpot "outside the Uzbek restaurant";
}