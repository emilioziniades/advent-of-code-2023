# Day 20

# Part 1

So, today I am trying to learn about the `State` monad, since this puzzle is clearly an instance of state transformations. But ... I am lost.

Eh, after spending a few hours reading about this, I just don't understand it. I get that `State s a` is just `s -> (s, a)`. There is `put` `get` and `return`.

The part that is confusing me is when I need to use `a` to update the `a`. This seems like it's not part of the mental model of the State monad, so I'm scrapping this whole idea and just working on the existing code.

# Part 2

Simply simulating button presses doesn't work for part two.
It takes an eternity to run, and I assume there is an exponential explosion.

So, instead, I am going to try working backwards.

The aim is to figure out how many button presses until `rx` receives a low pulse.
I have this gut feeling that it's possible to calculate it instead of simulating it.

Take the simplest possible 'circuit':

```
broadcaster -> rx
```

It takes one button press.

Consider the next simplest possibility:

```
broadcaster -> a
%a -> rx
```

It takes two presses.
The first press turns `a` on and sends `rx` a high pulse.
The second press turns `a` off and sends `rx` a low pulse.

What about two flipflop modules in a row?

```
broadcaster -> a
%a -> b
%b -> rx
```

It takes four presses.
The first press turns `a` on and sends `b` a high pulse. `b` does nothing.
The second press turns `a` off and sends `b` a low pulse. `b` turns on and sends `rx` a high pulse.
The third press turns `a` on and sends `b` a high pulse. `b` does nothing.
The fourth press turns `a` off and sends `b` a low pulse. `b` turns off and sends `rx` a low pulse.

Now, what about three flipflop models? My guess will be eight presses.

```
broadcaster -> a
%a -> b
%b -> c
%c -> rx
```

`c` receives its first low pulse after four presses and turns on.
After another four presses `c` receives its second low pulse and turns off, sending `rx` a low pulse.

So for a setup such as `broadcaster -> %a -> %b -> %c -> rx`, the calculation is `1 * 2 * 2 * 2`.
This can be calculated using recursion.

Now, the hard part is conjunction modules. My initial hunch is that it has something to do with LCMs.

The simplest example with conjunction modules is not that interesting.

```
broadcaster -> a, b
%a -> c
%b -> c
&c -> rx
```

The first press turns `a` and `b` on and both send `c` a high pulse. Then `c` sends rx a low pulse.
Only one button press.

What about something more interesting?

```
broadcaster -> aa, b
%aa-> ab
%ab -> c
%b -> c
&c -> rx
```

The first press turns `aa` and `b` on.
`ab` does not activate.
`b` sends a high pulse to `c`.
`c` remembers low pulse for `ab` and high pulse for `b`.

The first press turns `aa` and `b` off.
Both send low pulses.
`ab` turns on and sends a high pulse to `c`.
`b` turns off and sends a low pulse to `c`

NOW, before `b` sends its low pulse to `c`, `c`

---

OK, thinking about conjunction modules again, they are hard but not impossible.

The catch is that conjunction modules pass on signal when they receive all _high_ pulses, not low pulses.

Consider this simple setup:

```
broadcaster -> a
%a -> b
&b -> rx
```

It takes two presses for `a` to send `b` a low signal, but only one press for `a` to send `b` a high signal.

What about something similar?

```
broadcaster -> a
%a -> b
%b -> c
&c -> rx
```

It takes four presses for `b` to send `c` a low signal. But only two presses for `b` to send `c` a low signal.

And if I keep going?

```
broadcaster -> a
%a -> b
%b -> c
%c -> d
&d -> rx
```

It takes eight presses for `c` to send `d` a low signal, but only four presses for `c` to send `d` a high signal.

1. low to a, high to b
2. low to a, low to b, high to c
3. low to a, high to b
4. low to a, low to b, low to c, high to d

So, the rule seems that it's `nHigh =  nLow / 2`.

This seems to work, but I'm back to this case which is confusing me.

```
broadcaster -> a, c
%a -> b
%b -> d
%c -> d
&d -> rx
```

It's a case where two flipflop chains of different lengths feed into a conjunction.

1.

```
broadcaster -low-> a
broadcaster -low-> c
a -high-> b
c -high-> d
```

2.

```
broadcaster -low-> a
broadcaster -low-> c
a -low-> b
b -high-> d
c -low-> d
```

3.

```
broadcaster -low-> a
broadcaster -low-> c
a -low-> b
b -high-> d
c -low-> d
```
