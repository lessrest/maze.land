concrete MazeSwe of Maze =
  open SyntaxSwe, ParadigmsSwe, ConstructorsSwe
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

    North = mkPrep "norr om";
    South = mkPrep "söder om";
    West  = mkPrep "väst om";
    East  = mkPrep "öster om";

    Small = mkAP (mkA "liten");
    Large = mkAP (mkA "stor");

    Market = mkNP theSg_Det (mkN "centralmarknad");
    T17 = mkNP (mkN "T17");
    Synagogue = mkNP thePl_Det (mkN "synagog" (mkN "ruin" "ruiner"));
    Terapija = mkNP (mkN "Terapija");
    University = mkNP theSg_Det
      (mkN "universitet" "universitetet" "universitet" "universiteten");

    Cat = mkCN (mkN "katt");
    Dog = mkCN (mkN "hund");

    YIsDoorFromX dst how src
      = mkCl dst (ConstructorsSwe.mkAdv how src);

    AnXIsAtY item spot
      = mkCl (mkNP a_Det item) (ConstructorsSwe.mkAdv in_Prep spot);
}