concrete MazeLav of Maze =
  open SyntaxLav, ParadigmsLav, ConstructorsLav, ResLav
in {
  flags coding = utf8;
  lincat
    Spot = NP;
    Item = CN;
    Fact = Cl;
    Door = Prep;
    Line = Utt;
    Prop = AP;
    Fail = Utt;
  lin
    FactLine x = mkUtt x;
    PropItem prop item = mkCN prop item;

    North = mkPrep "uz ziemeļiem no" Gen Dat;
    South = mkPrep "uz dienvidiem no" Gen Dat;
    West  = mkPrep "uz rietumiem no" Gen Dat;
    East  = mkPrep "uz austrumiem no" Gen Dat;

    Small = mkAP (mkA "mazs");
    Large = mkAP (mkA "liels");

    Market = mkNP theSg_Det (mkN "centrāltirgus");
    T17 = mkNP (mkN "T17");
    Synagogue = mkNP thePl_Det (mkN "sinagogas drupas");
    Terapija = mkNP (mkN "Terapija");
    University = mkNP theSg_Det (mkN "universitāte");

    Cat = mkCN (mkN "kaķis");
    Dog = mkCN (mkN "suns");

    YIsDoorFromX dst how src
      = mkCl dst (ConstructorsLav.mkAdv how src);

    AnXIsAtY item spot
      = mkCl (mkNP a_Det item) (ConstructorsLav.mkAdv in_Prep spot);

    DoorConflict src dst fst snd
      -- "T17 is both north of Terapija and south of Terapija."
      = mkUtt
         (mkCl src
           (ConstructorsLav.mkAdv both7and_DConj
             (ConstructorsLav.mkAdv fst dst)
             (ConstructorsLav.mkAdv snd dst)));
}