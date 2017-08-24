concrete MazeLav2 of Maze =
  open Prelude in
{
  flags coding = utf8;

  lincat
    Line = SS;
    Spot = Str;
    Deed = SS;
    Item = SS;
    Door = SS;
    Wish = Str;

  lin
    North = { s = "x" };
    -- South = mkDoor "dienvidiem no";
    -- West  = mkDoor "rietumiem no";
    -- East  = mkDoor "austrumiem no";

    -- NorthWest = mkDoor "ziemeļrietumiem no";
    -- SouthWest = mkDoor "dienvidrietumiem no";
    -- NorthEast = mkDoor "ziemeļaustrumiem no";
    -- SouthEast = mkDoor "dienvidaustromiem no";

    -- Watermelon = mkCN (mkN "arbūzs");
    -- Euro = mkCN (mkN "eiro");

    -- Player = { s = "

--    DeedWish deed = mkUtt deed;

    -- SimpleShoppingDeed a b =
    --   mkVP
    --     (mkVP buy_V2 a)
    --     (mkAdv par_Prep b);

    SimpleWalkingDeed _ b = { s = b };
    DeedWish deed = deed.s;
}