# Day 22

## Part 1

I can simulate the falling cubes. Now I need to figure out which cubes can be disintegrated.

Here's one idea.

For each cube:

- First find the cubes directly above it
- Try and drop each of those cubes without the current cube present.
- If any cubes can drop, then the cube can't be disintegrated.

Another way to do it: generate dictionary of "dependencies" where each key is the current cube,
and the the value is an array of cubes the current cube depends on. If any cube depends on a
single cube, then that dependent cube can't be disintegrated

Coming back to this after a few days, and I have a much simpler idea. Consider a poorly
optimized algorithm: take every cube and compare it to every other cube, if the one is
on top of the other, record the dependency. This is bad because it's N^2 in the number of
cubes. My idea is to do this, but to sort the cubes by their height (z-axis), and for each
cube at height x, check cubes at height x + 1.
This will work, but another gotcha: what about cubes whose start and end heights are different?
if you're going from bottom up, then only the start matters. But there could be a cube higher up
that is sitting on that cube's end point and this algorithm wouldn't work.....

not sure about the algorithm, but ultimately what I want is a `Map Cube [Cube]`, and then figure out
the result from that. Which way around though? x ---depends-on---> ys or x ---is-depended-on-by---> ys
Now that I see it clearly like that, it's definitely the first option.
