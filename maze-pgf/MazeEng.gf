concrete MazeEng of Maze = MazeI with
  (Syntax = SyntaxEng)
** open
  ParadigmsEng,
  LexiconEng,
  ExtraEng,
  ConjunctionEng
in {

  lin
    North = mkDoor "north";
    South = mkDoor "south";
    West  = mkDoor "west";
    East  = mkDoor "east";
    NorthEast = mkDoor "northeast";
    NorthWest = mkDoor "northwest";
    SouthEast = mkDoor "southeast";
    SouthWest = mkDoor "southwest";

  oper mkDoor : Str -> Door;
  oper mkDoor s = lin Door
    { prep = mkPrep (s ++ "from")
    ; adv = lin Adv { s = s }
    };

  lin
    Watermelon = mkCN (mkN "watermelon");
    Knife = mkCN (mkN "knife");

  lin
    ItemConsumption item = mkVP (mkV2 spend_V) item;
    ItemPresumption item = mkVP (mkV2 use_V) item;
    ItemAcquisition item = mkVP (mkV2 get_V) item;
    FactConsumption fact =
      mkVP (mkV2 spend_V) (mkNP the_Det (mkCN (mkCN (mkN "fact")) (mkS fact)));
    FactPresumption fact =
      mkVP (mkV2 use_V) (mkNP the_Det (mkCN (mkCN (mkN "fact")) (mkS fact)));
    FactAcquisition fact =
      mkVP (mkV2 get_V) (mkNP the_Det (mkCN (mkCN (mkN "fact")) (mkS fact)));

  oper
    get_V = mkV "get";
    spend_V = mkV "spend";
    use_V = mkV "use";

  lin
    SellDeed item = mkVP (mkV2 (mkV "sell")) item;

    ConnectDeed src how dst =
      mkVP (mkV2V (mkV "make") noPrep to_Prep) dst (mkVP (mkAdv how.prep src));

    SimpleShoppingDeed a b =
      mkVP
        (mkVP buy_V2 a)
        (mkAdv for_Prep b);

    FactItem fact =
      mkNP the_Det (mkCN (mkCN (mkN "fact")) (mkS fact));

    RuleApplies rule = mkCl (mkVP (mkVS (mkV "happen")) rule);

    WhileRule fact pred item =
      mkS (mkAdv when_Subj (mkS fact))
        (mkS if_then_Conj
          (mkS (mkCl you_NP pred))
          (mkS (mkCl you_NP (mkVP (mkV2 get_V) item))));

    Player = you_NP;

  lin
    RuleLine rule = { s = "rule:" ++ (mkUtt rule).s };

    Euro = mkCN (mkN "euro" "euros");

    TryTo deed = { s = (mkUtt singularImpForm positivePol (mkImp deed)).s };
    Did deed = { s = (mkS (mkCl you_NP deed)).s };

}