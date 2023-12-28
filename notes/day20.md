# Day 20

# Part 1

So, today I am trying to learn about the `State` monad, since this puzzle is clearly an instance of state transformations. But ... I am lost.

Eh, after spending a few hours reading about this, I just don't understand it. I get that `State s a` is just `s -> (s, a)`. There is `put` `get` and `return`.

The part that is confusing me is when I need to use `a` to update the `a`. This seems like it's not part of the mental model of the State monad, so I'm scrapping this whole idea and just working on the existing code.
