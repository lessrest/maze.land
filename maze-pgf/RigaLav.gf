concrete RigaLav of Riga = MazeLav **
  open SyntaxLav, ParadigmsLav, LexiconLav
in {
  flags coding = utf8;
  oper
    mkSpot : Str -> NP;
    mkSpot s = mkNP (mkPN s);

    mkTheSpots : Str -> NP;
    mkTheSpots s = mkNP thePl_Det (mkN s);

    namedKind : N -> NP -> NP;
    namedKindPlural : N -> NP -> NP;

    namedKind kind name = mkNP the_Det (mkCN kind name);
    namedKindPlural kind name = mkNP thePl_Det (mkCN kind name);

    street_NP : Str -> NP;
    street_NP s = namedKind (mkN s) (mkNP (mkN "iela"));

    street_CN : Str -> CN;
    street_CN s = mkCN (mkN s) (mkNP (mkN "iela"));

  lin
    spot_Agroprojekts = mkSpot "\"Agroprojekts\"";
    spot_Banuzis = mkSpot "Bānūzis";
    spot_CafeRosemary = mkNP (mkCN (mkN "kafejnica") (mkNP (mkPN "Rozmarīns")));
    spot_DagdasStreet = street_NP "Dagdas";
    spot_Deficits = mkSpot "\"Deficīts\"";
    spot_GoodwillStudio = mkSpot "\"Goodwill Studio\"";
    spot_Idioma = mkSpot "\"Idioma\"";
    spot_Latgalite = mkSpot "\"Latgalīte\"";
    spot_MazaKrastaStreet = street_NP "Maza Krasta";
    spot_Pushkin11 = mkSpot "Pushkin 11";
    spot_SpekaStreet = street_NP "Speka";
    -- spot_Spikeri = mkNP (mkN "Spīķeris" plural);
    spot_SviestaPika = mkNP (mkCN (mkA "sviesta") (mkN "pika"));
    spot_TheAbrenesStreetBusStation = mkNP the_Det (mkCN (street_CN "Abrenes") (mkNP (mkN "autoosta")));
    spot_TheBookShopsJanusAndGora = mkNP thePl_Det (mkCN (mkN "book shop") (mkNP and_Conj (mkNP (mkPN "\"Janus\"")) (mkNP (mkPN "\"Gora\""))));
    spot_TheBusStation = mkNP (mkN "autoosta");
    spot_TheCentralMarketStands = mkNP thePl_Det (mkN "tirgus");
    spot_TheCentralMarketShoemaker = namedKind (mkN "kurpnieka") (mkNP (mkN "darbnīca"));
    spot_TheDaugavaSwimmingSpot = mkSpot "peldvieta Nr. 1";
    spot_TheFreeRigaNeighborhoodResidence = namedKind (mkN "Free Riga") (mkNP (mkN "rezidence"));
    spot_TheGhettoMuseum = namedKind (mkN "geto") (mkNP (mkN "muzejs"));
    spot_TheGreenField = mkNP (mkCN green_A field_N);
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
    spot_TheSpikeriPromenade = mkSpot "spīķeru promenade";
    spot_TheSynagogueMemorial = mkSpot "the synagogue memorial";
    spot_TheTunnel = mkNP the_Det coat_N;
    spot_TheVeraMuhinasMemorialHouse = mkSpot "the Vera Muhinas memorial house";
    spot_VingrumaClub = mkSpot "Vingruma Club";
    spot_TheMilkPavilion = mkSpot "the milk pavilion";
    spot_TheEmptyPavilion = mkSpot "the empty pavilion";
    spot_TheVegetablePavilion = mkSpot "the vegetable pavilion";
    spot_TheFishPavilion = mkSpot "the fish pavilion";
    spot_OutsideTheUzbekPlace = mkSpot "outside the Uzbek restaurant";

  oper
    field_N = mkN "lauks";
}