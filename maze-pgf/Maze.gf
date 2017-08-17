abstract Maze = Numeral ** {
  flags startcat = Line;
  cat
    Spot; -- A location, e.g. "the market"
    Door; -- A connection, e.g. "west"
    Item; -- A thing, e.g. "a small cat"
    Kind; -- A kind of thing, e.g. "cat"
    Prop; -- A property of a thing, e.g. "small"
    Some; -- A cardinality, e.g. "4"
    Fact; -- A state of affairs, e.g. "the market is east of the river"
    Wish; -- An intention, e.g. "go west"
    Line; -- An output sentence, e.g. "you see a small cat"
    Fail; -- An error, e.g. "X is both north and south of Y"
    Rule; -- A possibility, e.g. "one can spend 4 euros to get a burger"

  fun
    North, South, West, East : Door;

    YIsDoorFromX : Spot -> Door -> Spot -> Fact;
    AnXIsAtY     : Item -> Spot -> Fact;

    Cat, Dog, Watermelon, Bike : Kind;
    Euro : Kind;

    Small, Large : Prop;

    Count : Some -> Kind -> Item;
    Many : Kind -> Item;

    Both : Item -> Item -> Item;

    PropKind : Prop -> Kind -> Kind;

    FactLine : Fact -> Line;
    YouSeeX : Item -> Line;

    RuleLine : Rule -> Line;
    SpotRuleLine : Spot -> Rule -> Line;

    DoorConflict : Spot -> Spot -> Door -> Door -> Fail;

    Walk : Door -> Spot -> Spot -> Wish;
    WishLine : Wish -> Line;

    ConsumptionRule : Item -> Item -> Rule;
--    PresumptionNeed : Item -> Item -> Rule;

    Some1, Some2, Some3, Some4 : Some;
    SomeNumber : Numeral -> Some;
}
