# Kosmos

Kosmos implements semantics for a grammar of simple game worlds.

Worlds are defined by spots, items, and rules.

An item can be in a spot, and a rule can apply in a spot.

Rules have some different forms:

  * "You can pet cats."
  * "In the park, you can pet dogs."
  * "You can take flowers."
  * "In the market, you can spend four euros to get a big watermelon."

Rules relate conditions ("needs") to consequences ("deeds").

  * If in a spot where there is a cat,
    you can pet that cat.
  * If in the park
    and if there is a dog in the park,
    you can pet that dog.
  * If in a spot where there is a flower,
    you can move that flower to your pack.
  * If in the market
    and if there are four euros in your pack,
    you can delete those euros
    and get a watermelon in your pack.
