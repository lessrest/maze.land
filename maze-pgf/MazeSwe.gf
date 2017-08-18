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

  lin
    Production item = mkVP (mkV2 get_V) item;
    Consumption item = mkVP (mkV2 spend_V) item;
    Presumption item = mkVP (mkV2 use_V) item;
  oper
    get_V = mkV "få";
    spend_V = mkV "spenderar";
    use_V = mkV "använder";

  lin
    FactItem fact =
      mkNP the_Det (mkCN (mkCN fact_N) (mkS fact));
  oper
    fact_N = mkN "faktum" "faktumet" "fakta" "faktumen";
}