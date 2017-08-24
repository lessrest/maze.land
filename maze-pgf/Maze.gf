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

    Core1;  -- An abstract core rule
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

    ItemConsumption : Item -> Core1;
    ItemPresumption : Item -> Core1;
    ItemAcquisition : Item -> Core1;

    FactConsumption : Fact -> Core1;
    FactPresumption : Fact -> Core1;
    FactAcquisition : Fact -> Core1;

    -- WhileRule : Fact -> Need -> Item -> Rule;

    GeneralRule2 : Core1 -> Core1 -> Rule;
    GeneralRule3 : Core1 -> Core1 -> Core1 -> Rule;
    GeneralRule4 : Core1 -> Core1 -> Core1 -> Core1 -> Rule;

    SimpleShoppingDeed : Item -> Item -> Deed;
    SimpleWalkingDeed : Door -> Spot -> Deed;

    BuyDeed : Item -> Deed;
    EatDeed : Item -> Deed;
    SellDeed : Item -> Deed;
    GoDeed  : Door -> Deed;
    ConnectDeed : Spot -> Door -> Spot -> Deed;

    Did   : Deed -> Line;
    TryTo : Deed -> Line;

    Some1, Some2, Some3, Some4 : Some;
    SomeNumber : Numeral -> Some;

    DoorConflict : Spot -> Spot -> Door -> Door -> Fail;

    North, South, West, East : Door;
    NorthWest, SouthEast, SouthWest, NorthEast : Door;
    Euro, Cat, Dog, Watermelon, Bike : Kind;
    Knife : Kind;
    Small, Large : Prop;
}
