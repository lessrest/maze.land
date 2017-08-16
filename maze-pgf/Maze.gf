abstract Maze = {
  flags startcat = Line;
  cat
    Spot; Door; Item; Fact; Line; Prop; Fail;
  fun
    North, South, West, East : Door;

    YIsDoorFromX : Spot -> Door -> Spot -> Fact;
    AnXIsAtY     : Item -> Spot -> Fact;

    Market, T17, Synagogue, Terapija, University : Spot;

    Cat, Dog : Item;

    Small, Large : Prop;

    PropItem : Prop -> Item -> Item;
    FactLine : Fact -> Line;

    DoorConflict : Spot -> Spot -> Door -> Door -> Fail;
}
