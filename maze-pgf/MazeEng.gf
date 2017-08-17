concrete MazeEng of Maze = MazeI with (Syntax = SyntaxEng)
  ** open ParadigmsEng, LexiconEng
in {
  lin
    North = mkDoor "north";
    South = mkDoor "south";
    West  = mkDoor "west";
    East  = mkDoor "east";

  oper mkDoor : Str -> Door;
  oper mkDoor s = lin Door (mkPrep s);

  lin
    Watermelon = mkCN (mkN "watermelon");
}