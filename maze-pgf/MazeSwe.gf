concrete MazeSwe of Maze = MazeI with (Syntax = SyntaxSwe)
  ** open ParadigmsSwe, LexiconSwe
in {
  flags coding = utf8;
  lin
    North = mkDoor "norrut från";
    South = mkDoor "söderut från";
    West  = mkDoor "västerut från";
    East  = mkDoor "österut från";

  oper mkDoor : Str -> Door;
  oper mkDoor s = lin Door (mkPrep s);

  lin
    Watermelon = mkCN (mkN "vattenmelon" "vattenmeloner");

    Euro = mkCN (mkN "euro" "euros");
}