concrete MazeLav of Maze = MazeI with (Syntax = SyntaxLav)
  ** open ParadigmsLav, ResLav, LexiconLav
in {
  flags coding = utf8;

  oper mkDoor : Str -> Door;
  oper mkDoor s = lin Door (mkPrep ("uz" ++ s) Gen Dat);

  lin
    North = mkDoor "ziemeļiem";
    South = mkDoor "dienvidiem";
    West  = mkDoor "rietumiem";
    East  = mkDoor "austrumiem";

    Watermelon = mkCN (mkN "arbūzs");
}