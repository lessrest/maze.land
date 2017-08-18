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
    Deed;  -- An action, e.g. "go west" or "say hello"
    Rule;  -- A possibility, e.g. "spend 4 euros and get a burger"

    Core;  -- An abstract core rule

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

    -- Walk : Door -> Spot -> Spot -> Deed;

    FactItem : Fact -> Item;
    Consumption : Item -> Deed;
    Presumption : Item -> Deed;

    WhileRule : Fact -> Deed -> Item -> Rule;

    CoreRule2 : Deed -> Fact -> Rule;

    Trivial : Core;
    Keeping : Fact -> Core -> Core;
    Taking  : Fact -> Core -> Core;
    Giving  : Fact -> Core -> Core;
    Doing   : Deed -> Core -> Core;

    Some1, Some2, Some3, Some4 : Some;
    SomeNumber : Numeral -> Some;

    DoorConflict : Spot -> Spot -> Door -> Door -> Fail;

    North, South, West, East : Door;
    Euro, Cat, Dog, Watermelon, Bike : Kind;
    Small, Large : Prop;
}
