abstract Maze = Numeral ** {
  flags startcat = Line;

  cat
    Line;  -- A sentence, e.g. "you see a small cat"
    Fail;  -- An error, e.g. "X is both north and south of Y"

    Spot;  -- A location, e.g. "the market"
    Door;  -- A connection, e.g. "west"
    Item;  -- A thing, e.g. "a small cat"

    Kind;  -- A kind of thing, e.g. "cat"
    Prop;  -- A property of a thing, e.g. "small"
    Some;  -- A cardinality, e.g. "4"

    Fact;  -- A state of affairs, e.g. "the market is east of the river"
    Need;  -- A rule demand, e.g. "spend 1 euro"
    Rule;  -- A possibility, e.g. "spend 4 euros and get a burger"

    Core;  -- An abstract core rule
    Deed;  -- A grammatical expression of a rule (?)
    Wish;  -- A command

  fun
    Count : Some -> Kind -> Item;
    Both : Item -> Item -> Item;

    PropKind : Prop -> Kind -> Kind;

    Player : Item;
    YouSee : Item -> Line;
    FactLine : Fact -> Line;
    RuleLine : Rule -> Line;

    SpotHasDoor : Spot -> Door -> Spot -> Fact;
    SpotHasItem : Spot -> Item -> Fact;
    YouHaveItem : Item -> Fact;
    RuleApplies : Rule -> Fact;

    Consumption : Item -> Need;
    Presumption : Item -> Need;

    WhileRule : Fact -> Need -> Item -> Rule;

    Trivial : Core;
    Keeping : Fact -> Core -> Core;
    Taking  : Fact -> Core -> Core;
    Giving  : Fact -> Core -> Core;

    SimpleShoppingDeed : Item -> Item -> Deed;
    SimpleWalkingDeed : Door -> Spot -> Deed;
    DeedWish : Deed -> Wish;

    Some1, Some2, Some3, Some4 : Some;
    SomeNumber : Numeral -> Some;

    DoorConflict : Spot -> Spot -> Door -> Door -> Fail;

    North, South, West, East : Door;
    NorthWest, SouthEast, SouthWest, NorthEast : Door;
    Euro, Cat, Dog, Watermelon, Bike : Kind;
    Small, Large : Prop;
}
