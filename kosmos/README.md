# Kosmos

This project is basically a combination of

 * the world definition paradigm of Graham Nelson's [Inform 7](http://inform7.com/) (2006),
 * the narrative scheme of Chris Martens's [*Programming Interactive Worlds With Linear Logic*](https://www.cs.cmu.edu/~cmartens/thesis/) (2015), and
 * the linguistic technology of [Grammatical Framework](https://www.grammaticalframework.org) (1998).

It is motivated by a neighborhood exploration project in the "Lastadija"
area of Rīga, Latvia, undertaken by [Free Riga](https://www.freeriga.lv/).

## World descriptions

Kosmos implements semantics for a grammar of simple game worlds
defined by *spots*, *items*, and *rules*.

Rules have some different forms:

  * "You can pet cats."
  * "In the park, you can pet dogs."
  * "You can take flowers."
  * "In the market, you can spend four euros for a big watermelon."
  * "In the market, you can go north to the old town."

## World simulation

The world grammar is defined with [GF](https://www.grammaticalframework.org).

We compile the grammar to a `.pgf` file which contains all the lexical data
necessary to parse world descriptions.

We also compile the grammar's abstract syntax into Haskell definitions.

The simulator then uses GF's Haskell support library to parse some particular
world into a set of initial facts and rules.

A loading step expands the rules of the world description into a more explicit
representation, for example inferring "Y is east of X" from "X is west of Y".

Gameplay then consists of applying rules to an environment of states of affairs,
according to the player's commands.

Commands, messages, and errors are also parsed and linearized using the world grammar.
