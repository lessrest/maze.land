concrete MazeEng of Maze = MazeI with
  (Syntax = SyntaxEng)
** open
  ParadigmsEng,
  LexiconEng,
  ExtraEng,
  ConjunctionEng
in {

  lincat
    Core = [S];
  lin
    Core2 x y = BaseS (mkUtt (mkCl you_NP x)) (mkUtt (mkCl you_NP y));
    CoreLine x = mkUtt (
      mkCl (
        mkNP a_Det (mkCN (mkCN (mkN "rule")) (ConjS and_Conj x))
      )
    );

  -- lincat
  --   Core = ListVPS;
  -- lin
  --   Core2 x y =
  --     BaseVPS
  --       (MkVPS (mkTemp futureTense simultaneousAnt) positivePol x)
  --       (MkVPS (mkTemp futureTense simultaneousAnt) positivePol y);
  --   CoreN x y =
  --     ConsVPS
  --       (MkVPS (mkTemp futureTense simultaneousAnt) positivePol x)
  --       y;
  --   CoreLine x =
  --     mkUtt (PredVPS you_NP (ConjVPS and_Conj x));

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
    -- Production item = mkVP (mkV2 get_V) item;
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

    -- WhileRule : Fact -> Deed -> Item -> Rule;
    WhileRule fact pred item =
      mkS (mkAdv when_Subj (mkS fact))
        (mkS if_then_Conj
          (mkS (mkCl you_NP pred))
          (mkS (mkCl you_NP (mkVP (mkV2 get_V) item))));

    -- Residence spot = mkCl you_NP (mkVP (mkAdv in_Prep spot));
    Player = mkNP the_Det (mkCN (mkN "player"));

  lin
    -- RuleLine rule =
    --   mkUtt (mkCl you_NP (mkVP can_VV rule));

    RuleLine rule = { s = "rule:" ++ (mkUtt rule).s };

    Euro = mkCN (mkN "euro" "euros");
}