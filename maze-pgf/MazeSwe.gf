concrete MazeSwe of Maze = MazeI with (Syntax = SyntaxSwe)
  ** open ParadigmsSwe, LexiconSwe, CommonScand
in {
  flags coding = utf8;
  lin
    North = mkDoor "norrut";
    South = mkDoor "söderut";
    West  = mkDoor "västerut";
    East  = mkDoor "österut";
    NorthWest = mkDoor "nordvästerut";
    SouthEast = mkDoor "sydösterut";
    SouthWest  = mkDoor "sydvästerut";
    NorthEast  = mkDoor "nordösterut";

    Player = you_NP;

  oper mkDoor : Str -> Door;
  oper mkDoor s = lin Door
    { prep = mkPrep (s ++ "från")
    ; adv = lin Adv { s = s }
    };

  lin
    Watermelon = mkCN (mkN "vattenmelon" "vattenmeloner");
    Euro = mkCN (mkN "euro" "euron" "euro" "eurosarna" utrum);

  lin
    Production item = mkVP (mkV2 get_V) item;
    Consumption item = mkVP (mkV2 spend_V) item;
    Presumption item = mkVP (mkV2 use_V) item;
  oper
    get_V = mkV "få";
    spend_V = mkV "spenderar";
    use_V = mkV "använder";


  lin
    SimpleShoppingDeed a b =
      mkVP
        (mkVP buy_V2 a)
        (mkAdv for_Prep b);

    TryTo deed = { s = (mkUtt singularImpForm positivePol (mkImp deed)).s };
    Did deed = { s = (mkS (mkCl you_NP deed)).s ! Main };

}