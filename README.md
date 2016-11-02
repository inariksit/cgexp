# Experiments on CG and expressivity

## Transforming regular expressions to CG grammars

### Explanation 

We convert regular expressions into CG rules. 
Assume an automaton like the following:

``` 
             adj
             ___
       det   \ ↙    n
-> s0 -----> s1 -----> (s2)
```

If our tag set is [det,adj,n], then we have *word cohorts* like the following:

```
"<w>"
	"det" det
	"adj" adj 
	"n"   n
```

In addition, we insert *state cohorts* between each word cohort:

```
"<s>"
	"s0" s0
	"s1" s1 
	"s2" s2 
```

For example, a sequence with two transitions would be modelled with the following "sentence", with two word cohorts, three state cohorts:

```
"<s>"
	"s0" s0
	"s1" s1 
	"s2" s2 
"<w>"
	"det" det
	"adj" adj
	"n"   n
"<s>"
	"s0" s0
	"s1" s1 
	"s2" s2 
"<w>"
	"det" det
	"adj" adj 
	"n"   n
"<s>"
	"s0" s0
	"s1" s1 
	"s2" s2 
```

Now that we have word and state cohorts in place, we form *rules* which remove tags depending on states, and states depending on tags.
The only possible outcome on a sequence of 2 word cohorts should be the following:

```
"<s>"
	"s0" s0
"<w>"
	"det" det
"<s>"
	"s1" s1 
"<w>"
	"n" n
"<s>"
	"s2" s2
```

-----

### Producing CG rules

Now we show (relevant parts of) the CG produced from the automaton in Figure 1.

```
TEMPLATE DetCtx = (-1 S0 LINK 2 S1) ;
TEMPLATE AdjCtx = (-1 S1 LINK 2 S1) ;
TEMPLATE NounCtx = (-1 S1 LINK 2 S2) ;
```

For each tag in the automaton's alphabet, we create a CG template. For this particular automaton, it's very straightforward: a determiner can be surrounded by `(s0-->s1)`, adjective by `(s1-->s1)`, and a noun by `(s1-->s2)`. If we had a larger automaton, with another transition on the symbol `det`, we would add that into our template like this: `TEMPLATE DetCtx = (-1 S0 LINK 2 S1) OR (-1 someState LINK 2 otherState)`. 

```
TEMPLATE S0Ctx = (-1 >>> LINK 2 1 Det) ;
TEMPLATE S1Ctx = (-1 Det OR Adj LINK 2 Adj OR Noun) ;
TEMPLATE S2Ctx = (-1 Noun) ;
```

Next, we create templates for the possible contexts of the states. The state `s0` is only allowed as the first cohort -- there are no transitions *to* it, so it cannot have any cohort as a predecessor. Symmetrically, the state `s2` is only allowed as the last cohort, so it cannot have a follower. But the state `s1` can have both predecessors and successors. We can enter `s1` from two states: `s0` or `s1`, and we can exit from it into `s1` or `s2`. 

These templates will be used in the actual rules later on.

### Getting started

Now some rules! The maximally ambiguous words start off as maximally ambiguous (duh), but we can get rid of some states trivially, just based on their place in the sentence.

```
BEFORE-SECTIONS

REMOVE S1 or S2 IF (-1 >>>) ;
REMOVE S0 or S1 IF (0 <<<) ;
```

In the first rule, we remove all non-starting states from the first state cohort. In the second rule, we remove all non-final states from the last state cohort. We use VISL CG-3 magic tags for BOS/EOS to match them; but as easily we could've written a contextual test `(NOT -1 Det OR Adj OR Noun).` 

After these rules have applied, we move on to the core of the grammar.


```
SECTION

# Remove tags NOT between certain states
REMOVE Det IF (NEGATE T:DetCtx) ;
REMOVE Adj IF (NEGATE T:AdjCtx) ;
REMOVE Noun IF (NEGATE T:NounCtx) ;

# Remove states between certain tags
REMOVE S0 IF (NEGATE T:S0Ctx) ;
REMOVE S1 IF (NEGATE T:S1Ctx) ;
REMOVE S2 IF (NEGATE T:S2Ctx) ;
```

There is one rule for each state and each symbol, and all of them have the same form: *remove* a reading if its only allowed context does *not* hold. We are using the `NEGATE` keyword from VISL CG-3, which negates the whole chain of conditions. `NEGATE T:DetCtx` means that if the context does *not* match `DetCtx`: either the previous state has no `s0` reading, or the following state has no `s1` reading, then we should remove the `det` reading.

If a template has multiple contexts, separated by ORs, `NEGATE` still treats it correctly. The negation of a template such as `(-1 S0 LINK 2 S1) OR (-1 someState LINK 2 otherState)` means "anything that is not `s0--s1`, nor `someState--otherState`".

#### Sidetrack: distributivity

In the templates for states, we don't need to enumerate all pairs: `(-1 Det OR Adj LINK 2 Adj OR Noun)` is equivalent to `(-1 Det LINK 2 Adj) OR (-1 Det LINK 2 Noun) OR (-1 Adj LINK 2 Adj) OR (-1 Adj LINK 2 Noun) ;`. But we do need to do so for the tag contexts: `TEMPLATE DetCtx = (-1 S0 LINK 2 S1) OR (-1 someState LINK 2 otherState)` is not equivalent to `TEMPLATE DetCtx = (-1 S0 OR someState LINK 2 S1 OR otherState)`, because the latter would match also the contexts `(-1 S0) (1 otherState)` and `(-1 someState) (1 S1)`


### Matching more than one word

If the regex gives more than one possible output for a sentence of n words, we have to model those in the CG output.

Let's have a little bit more complex automaton:

```
    Det
(0)----->1

  Verb
1------>(0)

 [Noun,Verb]
1----------->(2)

    Verb
(2)------>(0)

   [Det,Adj,Noun]
(2)-------------->1
```

From the automaton we see that a sequence of 2 transitions can take two paths:

```             Noun
        Det     Verb       
a.  (0) ----> 1 -----> (2) 

        Det     Verb
b.  (0) ----> 1 -----> (0)

```

Now when we run the example on a sequence with 3 state cohorts and 2 word cohorts, this is the result:

```
$ ./runExample.sh random 2 

"<s>"
	"s0" s0
"<w>"
	"det" det
"<s>"
	"s1" s1
"<w>"
	"verb" verb
	"noun" noun
"<s>"
	"s0" s0
	"s2" s2
```
