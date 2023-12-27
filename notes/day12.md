# Day 12

## Part 2

### A snag

I am using the part 1 puzzle to check my new approach.
They are smaller and can be compared to brute force.
This is the one I am currently stuck on.

`..####??#?#??#?.?.#. 4,4,2,1,1`

First, replace the sure things:

`..####.?#?#??#?.?.#.`

This is the same as the code:

`..####.?#?#??#?.?.#.`

However, the part that fails is creating the subproblems.
This is what I get when I create the subproblems by hand.

`..####.?#?#??#?.?.#. 4,4,2,1,1`

`[("####", [4]), ("?#?#??#?.?", [4, 2, 1]), ("#", [1])]`

There's probably a better way to do the subproblems but with caching this is good enough.
My code fails to generate those subproblems though.

### The reason for the snag

Oh god, I have run headfirst into something I was pondering a few days ago.
Here's the simplest failure case.

`.?.#. 1,1`

Assuming you can't replace sure things, you would expect the following subproblems:

`[("?", [1]), ("#", [1])]`

BUT, the code errors out. The issue lies in how I am creating these subproblems.
The general approach is to find the next sure thing, and create three subproblems:

1. everything before the sure thing
2. the sure thing
3. recursively do the same for everything after the sure thing.

The hard part is pairing the spring IDs with the actual spring.
So far what I've done is use the ID of that spring section to do the pairing.
I find the next sure thing, get its length (its ID) and then take all the IDs until that length.
All is fine when each ID is different, but if there are duplicates, the code goes boom.
It doesn't know how many IDs to pop off the front.
Does it go until the first `1`, or the second `1`? Each case will be different.

Sadly, this one actually works, but because of luck, not good engineering:

`.#.?. 1,1`

### Another approach

One thing I was thinking about is just trying all combinations of groupings, exiting early when any one spring-springId pair has zero possibilities.
This actually might work if the performance hit isn't too hectic.

Take the above example after replacing sure things.

`..####.?#?#??#?.?.#. 4,4,2,1,1`

It has three distinct groups of springs, and four spring IDs. How could we consider all groupings?

Call the groups A, B and C.
Call the spring IDs a, b, c, d.

A, [a, b, c, d] | B, [] | C, []

A, [a, b, c] | B, [d] | C, []
A, [a, b, c] | B, [] | C, [d]

A, [a, b] | B, [c, d] | C, []
A, [a, b] | B, [c] | C, [d]
A, [a, b] | B, [] | C, [c, d]

A, [a] | B, [b, c, d] | C, []
A, [a] | B, [b, c] | C, [d]
A, [a] | B, [b] | C, [c, d]
A, [a] | B, [] | C, [b, c, d]

A, [] | B, [a, b, c, d] | C, []
A, [] | B, [a, b, c] | C, [d]
A, [] | B, [a, b] | C, [c, d]
A, [] | B, [a] | C, [b, c, d]
A, [] | B, [] | C, [a, b, c, d]

Huh, this is getting kind of complicated and it's only a small example.

My gut feeling is that this is not going to work, it's going to grow exponentially as the problems get bigger. Back to the drawing board.

Let's consider this guy again.

`.?.#. 1,1` should give subproblems `[("?", [1]), ("#", [1])]`.

The previous approach, I'll call it "split on next best things" has the fatal flaw described above.
