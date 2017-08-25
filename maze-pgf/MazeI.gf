incomplete concrete MazeI of Maze = {
  lincat
    Line = { s : Str };
    Fail = Utt;

    Spot = { np : NP; prep: Prep };
    Door = { prep : Prep; adv : Adv };
    Item = NP;

    Kind = CN;
    Prop = AP;
    Some = Numeral;

    Fact = Cl;
    Need = VP;
    Rule = S;

    Deed = VP;

    Wish = Utt;
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

    SpotHasDoor dst how src = mkCl dst.np (mkAdv how.prep src.np);
    SpotHasItem spot item = mkCl item (mkAdv spot.prep spot.np);
    YouHaveItem item = mkCl you_NP have_V2 item;

    YouSee item  = mkUtt (mkCl you_NP see_V2 item);
    FactLine fact = mkUtt fact;
    DoorLine door spot =
      mkUtt (mkCl spot.np door.adv);

    DoorConflict src dst fst snd
      -- "T17 is both north of Terapija and south of Terapija."
      = mkUtt
         (mkCl src.np
           (mkAdv both7and_DConj
             (mkAdv fst.prep dst.np)
             (mkAdv snd.prep dst.np)));

    SimpleWalkingDeed _ b =
      mkVP (mkVP go_V) (mkAdv to_Prep b.np);

    BuyDeed item = mkVP buy_V2 item;
    EatDeed item = mkVP eat_V2 item;
    GoDeed door = mkVP (mkVP go_V) door.adv;

    GeneralRule2 a b =
      mkS and_Conj (youCore a) (youCore b);
    GeneralRule3 a b c =
      mkS and_Conj (mkListS (youCore a) (mkListS (youCore b) (youCore c)));
    GeneralRule4 a b c d =
      mkS and_Conj (mkListS (youCore a) (mkListS (youCore b) (mkListS (youCore c) (youCore d))));

  oper youCore : VP -> S;
  oper youCore x = mkS (mkCl you_NP x);

  -- Lexicon words
  lin
    Small = mkAP small_A;
    Large = mkAP big_A;

    Bike = mkCN bike_N;
    Cat = mkCN cat_N;
    Dog = mkCN dog_N;
}