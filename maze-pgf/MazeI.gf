incomplete concrete MazeI of Maze = {
  lincat
    Line = Utt;
    Fail = Utt;

    Spot = NP;
    Door = Prep;
    Item = NP;

    Kind = CN;
    Prop = AP;
    Some = Numeral;

    Fact = Cl;
    Deed = VP;
    Rule = S;

  lin

    Some1 = mkNumeral "1";
    Some2 = mkNumeral "2";
    Some3 = mkNumeral "3";
    Some4 = mkNumeral "4";

    Count some kind = mkNP some kind;
    Many kind = mkNP many_Det kind;
    Both x y = mkNP and_Conj x y;

    PropKind prop kind = mkCN prop kind;

    YIsDoorFromX dst how src = mkCl dst (mkAdv how src);
    AnXIsAtY item spot = mkCl item (mkAdv in_Prep spot);

    FactLine fact = mkUtt fact;
    RuleLine rule = mkUtt rule;
    YouSeeX item = mkUtt (mkCl you_NP see_V2 item);

    Rule1 deed = mkS (mkCl you_NP deed);
    Rule2 deed1 deed2 =
      mkS and_Conj
        (mkS (mkCl you_NP deed1))
        (mkS (mkCl you_NP deed2));

    -- Walk door src dst
    --   = mkImp
    --       (mkVP
    --         (mkVP (mkVP walk_V) (mkAdv door src))
    --         (mkAdv to_Prep dst));

    DoorConflict src dst fst snd
      -- "T17 is both north of Terapija and south of Terapija."
      = mkUtt
         (mkCl src
           (mkAdv both7and_DConj
             (mkAdv fst dst)
             (mkAdv snd dst)));

  -- Lexicon words
  lin
    Small = mkAP small_A;
    Large = mkAP big_A;

    Bike = mkCN bike_N;
    Cat = mkCN cat_N;
    Dog = mkCN dog_N;
}