incomplete concrete MazeI of Maze = {
  lincat
    Line = { s : Str };
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

    Core1 = VP;

  lin

    Some1 = mkNumeral "1";
    Some2 = mkNumeral "2";
    Some3 = mkNumeral "3";
    Some4 = mkNumeral "4";

    Count some kind = mkNP some kind;
    -- Many kind = mkNP many_Det kind;
    Both x y = mkNP and_Conj x y;

    PropKind prop kind = mkCN prop kind;

    SpotHasDoor dst how src = mkCl dst (mkAdv how src);
    SpotHasItem spot item = mkCl item (mkAdv in_Prep spot);
    YouHaveItem item = mkCl you_NP have_V2 item;

    YouSee item  = mkUtt (mkCl you_NP see_V2 item);
    FactLine fact = mkUtt fact;

    Rule1 deed = deed;
    Rule2 deed1 deed2 =
      mkS and_Conj
        (mkS (mkCl you_NP can_VV deed1))
        (mkS (mkCl you_NP can_VV deed2));

    LocalRule1 spot deed =
      mkS (mkAdv in_Prep spot) (mkS (mkCl you_NP can_VV deed));
    -- CoreRule core =
    --   mkS (mkCl you_NP can_VV core);

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