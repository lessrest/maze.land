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

    ConsumptionRule need boon =
      mkVP (mkV2V (mkV "spend") noPrep to_Prep)
        need
        (mkVP (mkV2 "get") boon);

    RuleLine rule =
      mkUtt (mkCl you_NP (mkVP can_VV rule));

    SpotRuleLine spot rule =
      mkUtt (mkS (mkAdv in_Prep spot) (mkS (mkCl you_NP (mkVP can_VV rule))));

    Euro = mkCN (mkN "euro" "euros");
}