concrete MazeSwe of Maze = MazeI with (Syntax = SyntaxSwe)
  ** open ParadigmsSwe, LexiconSwe
in {
  flags coding = utf8;
  lin
    North = mkDoor "norr";
    South = mkDoor "söder";
    West  = mkDoor "väster";
    East  = mkDoor "öster";

  oper mkDoor : Str -> Door;
  oper mkDoor s = lin Door (mkPrep s);

  lin
    Watermelon = mkCN (mkN "vattenmelon" "vattenmeloner");
}