concrete MazeEng of Maze = MazeI with
  (Syntax = SyntaxEng)
** open
  ParadigmsEng,
  LexiconEng
in {
  lin
    North = mkDoor "north from";
    South = mkDoor "south from";
    West  = mkDoor "west from";
    East  = mkDoor "east from";

  oper mkDoor : Str -> Door;
  oper mkDoor s = lin Door (mkPrep s);

  lin
    Watermelon = mkCN (mkN "watermelon");

  lin
    Production item = mkVP (mkV2 get_V) item;
    Consumption item = mkVP (mkV2 spend_V) item;
    Presumption item = mkVP (mkV2 use_V) item;
  oper
    get_V = mkV "get";
    spend_V = mkV "spend";
    use_V = mkV "use";

  lin
    FactItem fact =
      mkNP the_Det (mkCN (mkCN (mkN "fact")) (mkS fact));

    RuleApplies rule = mkCl (mkVP (mkVS (mkV "hold")) rule);

  lin
    RuleLine rule =
      mkUtt (mkCl you_NP (mkVP can_VV rule));

    Euro = mkCN (mkN "euro" "euros");
}