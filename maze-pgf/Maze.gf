abstract Maze = Numeral ** {
  flags startcat = Line;
  cat
    Line; -- A sentence, e.g. "you see a small cat"
    Fail; -- An error, e.g. "X is both north and south of Y"

    Spot; -- A location, e.g. "the market"
    Door; -- A connection, e.g. "west"
    Item; -- A thing, e.g. "a small cat"

    Kind; -- A kind of thing, e.g. "cat"
    Prop; -- A property of a thing, e.g. "small"
    Some; -- A cardinality, e.g. "4"

    Fact; -- A state of affairs, e.g. "the market is east of the river"
    Deed; -- An intention, e.g. "go west" or "spend 4 euros"
    Rule; -- A possibility, e.g. "spend 4 euros and get a burger"

  fun
    Count : Some -> Kind -> Item;
    Many : Kind -> Item;
    Both : Item -> Item -> Item;

    PropKind : Prop -> Kind -> Kind;

    YouSee : Item -> Line;

    SpotHasDoor : Spot -> Door -> Spot -> Fact;
    SpotHasItem : Spot -> Item -> Fact;
    RuleApplies : Rule -> Fact;

    Walk : Door -> Spot -> Spot -> Deed;

    FactItem : Fact -> Item;

    Production : Item -> Deed;
    Consumption : Item -> Deed;
    Presumption : Item -> Deed;

    Rule1 : Deed -> Rule;
    Rule2 : Deed -> Deed -> Rule;
    LocalRule1 : Spot -> Deed -> Rule;

    Some1, Some2, Some3, Some4 : Some;
    SomeNumber : Numeral -> Some;

    DoorConflict : Spot -> Spot -> Door -> Door -> Fail;

    North, South, West, East : Door;
    Euro, Cat, Dog, Watermelon, Bike : Kind;
    Small, Large : Prop;
}
