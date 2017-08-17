abstract Maze = {
  flags startcat = Line;
  cat
    Spot; Door; Item; Fact; Line; Prop; Fail;
    Kind; Wish;
  fun
    North, South, West, East : Door;

    YIsDoorFromX : Spot -> Door -> Spot -> Fact;
    AnXIsAtY     : Item -> Spot -> Fact;

    Cat, Dog, Watermelon, Bike : Kind;

    Small, Large : Prop;

    One : Kind -> Item;
    Many : Kind -> Item;
    PropKind : Prop -> Kind -> Kind;

    FactLine : Fact -> Line;
    YouSeeX : Item -> Line;

    DoorConflict : Spot -> Spot -> Door -> Door -> Fail;

    Walk : Door -> Spot -> Spot -> Wish;
    WishLine : Wish -> Line;
}
