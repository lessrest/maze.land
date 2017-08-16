concrete MazeEng of Maze =
  open SyntaxEng, ParadigmsEng, ConstructorsEng
in {
  lincat
    Spot = NP;
    Item = CN;
    Fact = Cl;
    Door = Prep;
    Line = Utt;
    Prop = AP;
  lin
    FactLine x = mkUtt x;
    PropItem prop item = mkCN prop item;

    North = mkPrep "north of";
    South = mkPrep "south of";
    West  = mkPrep "west of";
    East  = mkPrep "east of";

    Small = mkAP (mkA "small");
    Large = mkAP (mkA "large");

    Market = mkNP theSg_Det (mkN "central market");
    T17 = mkNP (mkN "T17");
    Synagogue = mkNP thePl_Det (mkN "synagogue ruin");
    Terapija = mkNP (mkN "Terapija");
    University = mkNP theSg_Det (mkN "university");

    Cat = mkCN (mkN "cat");
    Dog = mkCN (mkN "dog");

    YIsDoorFromX dst how src
      = mkCl dst (ConstructorsEng.mkAdv how src);

    AnXIsAtY item spot
      = mkCl (mkNP a_Det item) (ConstructorsEng.mkAdv in_Prep spot);
}