concrete RigaSwe of Riga = MazeSwe **
  open SyntaxSwe, ParadigmsSwe
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
    spot_CafeRosemary = mkSpot "Kafé Rosmarin";
    spot_DagdasStreet = mkSpot "Dagdasgatan";
    spot_Deficits = mkSpot "Deficits";
    spot_GoodwillStudio = mkSpot "Goodwill Studio";
    spot_Idioma = mkSpot "\"Idioma\"";
    spot_Latgalite = mkSpot "Latgalite";
    spot_MazaKrastaStreet = mkSpot "Maza Krasta-gatan";
    spot_Pushkin11 = mkSpot "Pushkin 11";
    spot_SpekaStreet = mkSpot "Spekagatan";
    spot_Spikeri = mkSpot "Spikeri";
    spot_SviestaPika = mkSpot "Sviesta pika";
    spot_TheAbrenesStreetBusStation = mkSpot "busstationen på Abrenesgatan";
    spot_TheBookShopsJanusAndGora = mkNP thePl_Det (mkCN (mkN "bokhandlarna") (mkNP and_Conj (mkNP (mkPN "\"Janus\"")) (mkNP (mkPN "\"Gora\""))));
    spot_TheBusStation = mkSpot "busstationen";
    spot_TheCentralMarketPavilions = mkTheSpots "centralmarknadens paviljonger";
    spot_TheCentralMarketShoemaker = mkSpot "centralmarknadens skomakare";
    spot_TheDaugavaSwimmingSpot = mkSpot "simplatsen";
    spot_TheFreeRigaNeighborhoodResidence = mkSpot "Free Rigas grannskapsresidens";
    spot_TheGhettoMuseum = mkSpot "ghettomuseet";
    spot_TheGreenField = mkSpot "den stora gräsmattan";
    spot_TheHummusTeam = mkSpot "hummusgruppen";
    spot_TheIndustrialGoodsMarket = mkSpot "den industriella marknaden";
    spot_TheJesusChurch = mkSpot "Jesuskyrkan";
    spot_TheNightMarket = mkSpot "nattmarknaden";
    spot_TheOrthodoxChurch = mkSpot "ortodoxa kyrkan";
    spot_ThePan = mkSpot "\"pannan\"";
    spot_ThePushkinStreetPrintShop = mkSpot "Pushkingatans tryckeri";
    spot_TheRedCrossSocialCenter = mkSpot "röda korsets socialcenter";
    spot_TheRiversideGallery = mkSpot "strandgalleriet";
    spot_TheScienceAcademy = mkSpot "vetenskapsakademin";
    spot_TheSoyShop = mkSpot "sojaaffären";
    spot_TheSpikeriPromenade = mkSpot "Spikeripromenaden";
    spot_TheSynagogueMemorial = mkSpot "synagogemonumentet";
    spot_TheTunnel = mkSpot "tunneln";
    spot_TheVeraMuhinasMemorialHouse = mkSpot "Vera Muhinas minneshus";
    spot_VingrumaClub = mkSpot "Vingrumaklubben";
}