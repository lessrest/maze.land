concrete RigaLav2 of Riga = MazeLav2 ** open Prelude in {
  flags coding = utf8;
  oper
    spot : Str -> Str;
    spot s = s;

  lin
    spot_Agroprojekts = spot "Agroprojekts";
    spot_Banuzis = spot "Bānūzis";
    spot_CafeRosemary = spot "kafejnīca Rozmarīns";
    spot_DagdasStreet = spot "Dagdas iela";
    spot_Deficits = spot "Deficīts";
    spot_GoodwillStudio = spot "Goodwill Studio";
    spot_Idioma = spot "veikals \"Idioma\"";
    spot_Latgalite = spot "Latgalīte";
    spot_MazaKrastaStreet = spot "Maza Krasta iela";
    spot_Pushkin11 = spot "Puškina iela 11";
    spot_SpekaStreet = spot "Speķa iela";
    spot_Spikeri = spot "Spīķeri";
    spot_SviestaPika = spot "Sviesta pika";
    spot_TheAbrenesStreetBusStation = spot "Abrenes ielas pieturvieta";
    spot_TheBookShopsJanusAndGora = spot "Grāmatnīcas Janus un Gora";
    spot_TheBusStation = spot "autoosta";
    spot_TheCentralMarketStands = spot "centrāltirgus";
    spot_TheCentralMarketShoemaker = spot "kurpnieka darbnīca";
    spot_TheDaugavaSwimmingSpot = spot "peldvieta Nr. 1";
    spot_TheFreeRigaNeighborhoodResidence = spot "Free Riga rezidence (T17)";
    spot_TheGhettoMuseum = spot "geto muzejs";
    spot_TheGreenField = spot "zaļais laikums";
    spot_TheHummusTeam = spot "humusa komanda";
    spot_TheIndustrialGoodsMarket = spot "kurpnieka darbnīca";
    spot_TheJesusChurch = spot "Jēzus baznīca";
    spot_TheNightMarket = spot "nakts tirgus";
    spot_TheOrthodoxChurch = spot "Vissvētās Dievmātes Pasludināšanas pareizticīgo baznīca";
    spot_ThePan = spot "panna";
    spot_ThePushkinStreetPrintShop = spot "tipogrāfija Puškina iela 12";
    spot_TheRedCrossSocialCenter = spot "Sarkanā Krusta sociālais centrs Gaiziņs";
    spot_TheRiversideGallery = spot "krasta darbnīcas";
    spot_TheScienceAcademy = spot "Latvijas Zinātņu Akadēmija";
    spot_TheSoyShop = spot "sojas veikals";
    spot_TheSpikeriPromenade = spot "spīķeru promenade";
    spot_TheSynagogueMemorial = spot "Rīgas Lielās Horālās sinagogas memoriāls";
    spot_TheTunnel = spot "tunelītis";
    spot_TheVeraMuhinasMemorialHouse = spot "Veras Muhinas memoriālais dzīvoklis";
    spot_VingrumaClub = spot "Vingruma klubs";
    spot_TheMilkPavilion = spot "piens paviljons";
    spot_TheEmptyPavilion = spot "gastronomijas pavilons";
    spot_TheVegetablePavilion = spot "sakņu paviljons";
    spot_TheFishPavilion = spot "zivju paviljons";
    spot_OutsideTheUzbekPlace = spot "Uzbek restorans";
    spot_7Tram = spot "7. tramvajs";

  -- Questions!
  --
  --  1. "zivju paviljons" / "sakņu paviljons"
  --      6th declension genitive plural of fish
  --      5th declension genitive plural of root
  --
  --  2. "Vingruma klubs"
  --     PN -> NP -> NP?
  --
  --  3. "ej uz grāmatnīcām Janus, un Gora"?
}