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

    NorthWest = mkDoor "ziemeļrietumiem no";
    SouthWest = mkDoor "dienvidrietumiem no";
    NorthEast = mkDoor "ziemeļaustrumiem no";
    SouthEast = mkDoor "dienvidaustromiem no";

    Watermelon = mkCN (mkN "arbūzs");
    Euro = mkCN (mkN "eiro");
    Player = you_NP;

--    DeedWish deed = mkUtt deed;

    SimpleShoppingDeed a b =
      mkVP
        (mkVP buy_V2 a)
        (mkAdv par_Prep b);
}