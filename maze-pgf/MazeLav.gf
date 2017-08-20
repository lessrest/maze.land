concrete MazeLav of Maze = MazeI with (Syntax = SyntaxLav)
  ** open ParadigmsLav, ResLav, LexiconLav
in {
  flags coding = utf8;

  oper mkDoor : Str -> Door;
  oper mkDoor s = lin Door (mkPrep ("uz" ++ s) Gen Dat);

  lin
    North = mkDoor "ziemeļiem no";
    South = mkDoor "dienvidiem no";
    West  = mkDoor "rietumiem no";
    East  = mkDoor "austrumiem no";

    NorthWest = mkDoor "ziemeļrietumos no";
    SouthWest = mkDoor "dienvidrietumos no";
    NorthEast = mkDoor "ziemeļaustrumos no";
    SouthEast = mkDoor "dienvidaustromos no";

    Watermelon = mkCN (mkN "arbūzs");
    Euro = mkCN (mkN "euro");
    Player = you_NP;
}