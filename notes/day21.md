# Day 21

## Part 2

The solution is clearly too large to calculate 'by hand', so there must be some maths magic, or some dynamic programming/memoization.
I'm going to explore the first option.

Imagine there are no rocks blocking the way.

In one step you could reach these plots:
.....
..O..
.O.O.
..O..
.....

In two steps, you could reach these plots:
..O..
.O.O.
O.O.O
.O.O.
..O..

In three steps, you could reach these plots:
.........
....O....
...O.O...
..O.O.O..
.O.O.O.O.
..O.O.O..
...O.O...
....O....
.........

It is effectively a diamond shape alternating 'O' and '.' depending on whether the number of steps is even or odd.

How would you calculate this? Well it is two triangular numbers.

Tn = (n^2 + n) / 2

So 2Tn = n^2 + n

However, the triangle base is shared between the two triangles, so it's

2Tn - n = n^2

And in this case, n is the number of steps + 1.

So for steps = 1, total = (1+1)^2 = 2^2 = 4.
For steps = 2, total = (2+1)^2 = 3^2 = 9
For steps = 3, total = (3+1)^2 = 4^2 = 16

This is for the ideal case where there are no rocks blocking.
So, clearly with the rocks there will be less reachable plots. The question is, how many less?
Maybe it's just the total number of rocks that fall into that triangle.

Let's try this in the example input, for steps = 3.
...........
.....###.#.
.###.%#..#.
..#.%.O.#..
...O#O#O...
.#%.%S%#%#.
.##O.%.O.#.
....O.O##..
.##.#%####.
.##..##.##.
...........

'%' is "reachable but rock is there" and 'O' is "reachable". This does not work.
Sometimes there are spots that are theoretically reachable, but you would have to go through rocks to get there.
So this approach STILL overshoots the actual number of available plots.
