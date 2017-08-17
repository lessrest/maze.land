incomplete concrete MazeI of Maze = {
  lincat
    Spot = NP;
    Fact = Cl;
    Line = Utt;
    Prop = AP;
    Fail = Utt;
    Kind = CN;
    Item = NP;
    Wish = Imp;
    Door = Prep;
    -- Need = NP;
    Some = Numeral;
    Rule = VP;

  lin
    FactLine x = mkUtt x;
    WishLine wish = mkUtt wish;
    PropKind prop kind = mkCN prop kind;

    Many kind = mkNP many_Det kind;

    YouSeeX item
      = mkUtt (mkCl you_NP see_V2 item);
    AnXIsAtY item spot
      = mkCl item (mkAdv in_Prep spot);

    Small = mkAP small_A;
    Large = mkAP big_A;

    Bike = mkCN bike_N;
    Cat = mkCN cat_N;
    Dog = mkCN dog_N;

    Walk door src dst
      = mkImp
          (mkVP
            (mkVP (mkVP walk_V) (mkAdv door src))
            (mkAdv to_Prep dst));

    YIsDoorFromX dst how src
      = mkCl dst (mkAdv how src);

    DoorConflict src dst fst snd
      -- "T17 is both north of Terapija and south of Terapija."
      = mkUtt
         (mkCl src
           (mkAdv both7and_DConj
             (mkAdv fst dst)
             (mkAdv snd dst)));

    Some1 = mkNumeral "1";
    Some2 = mkNumeral "2";
    Some3 = mkNumeral "3";
    Some4 = mkNumeral "4";

    Both x y = mkNP and_Conj x y;
    Count some kind = mkNP some kind;
}