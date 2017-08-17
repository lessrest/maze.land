abstract Maze = Numeral ** {
  flags startcat = Line;
  cat
    Spot; -- A location, e.g. "the market"
    Door; -- A connection, e.g. "west"
    Item; -- A thing, e.g. "a small cat"
    Kind; -- A kind of thing, e.g. "cat"
    Prop; -- A property of a thing, e.g. "small"
    Fact; -- A state of affairs, e.g. "the market is east of the river"
    Wish; -- An intention, e.g. "go west"
    Line; -- An output sentence, e.g. "you see a small cat"
    Fail; -- An error, e.g. "X is both north and south of Y"
    Need; -- A prerequisite, e.g. "to spend 4 euros"
    Some; -- A cardinality, e.g. "4"
    -- Have; -- A possession, e.g. "wakefulness", "a burger", or "4 euros"
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

    ConsumptionNeed : Some -> Kind -> Need;
    PresumptionNeed : Some -> Kind -> Need;

    SomeNumber : Numeral -> Some;
}
