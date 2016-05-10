# cgexp

Random experiments on CG and expressivity.

So far just a simple program that converts very simple regular expressions into CG rules. 
We assume an automaton like

```     det        n
->(s0) ----->(s1)----->((s2))
```

Then we insert all the states in between the cohorts--it should work with actual sentences or symbolic sentences--and form rules which remove tags depending on states, and states depending on tags.

This is very experimental and probably wrong.

-----

```
$ ghci Rule.hs 
Ok, modules loaded: Rule, Automaton.
λ> test
REMOVE [Adj,Noun] IF (-1 [s0])
REMOVE [Det] IF (-1 [s0]) (NOT 1 [s1])
REMOVE [Det] IF (-1 [s1])
REMOVE [Adj] IF (-1 [s1]) (NOT 1 [s1])
REMOVE [Adj] IF (-1 [s1]) (NOT 1 [s2])
REMOVE [Noun] IF (-1 [s1]) (NOT 1 [s2])
REMOVE [Det,Adj,Noun] IF (-1 [s2])
```