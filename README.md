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

### Example

The following is a short, randomly generated automaton (using `randomAutomaton` in `Test.hs`):

```
$ runghc Rule.hs random

   [Det]
(0)----->1

 [Verb]
1------>(0)

 [Noun,Verb]
1----------->(2)

   [Verb]
(2)------>(0)

   [Det,Adj,Noun]
(2)-------------->1
```

This automaton produces the following CG: 

```
$ cat -n examples/random/random.rlx
    ...
     9	BEFORE-SECTIONS
    10	
    11	REMOVE (s1) OR (s2) IF (-1 >>>) ;
    12	REMOVE (s1) IF (0 <<<) ;
    13	
    14	SECTION
    15	
    16	# Remove tags NOT between certain states
    17	REMOVE Det IF (NOT -1 (s0) OR (s2)) ;
    18	REMOVE Det IF (NOT 1 (s1)) ;
    19	REMOVE Adj IF (NOT -1 (s2)) ;
    20	REMOVE Adj IF (NOT 1 (s1)) ;
    21	REMOVE Noun IF (NOT -1 (s1) OR (s2)) ;
    22	REMOVE Noun IF (NOT 1 (s2) OR (s1)) ;
    23	REMOVE Verb IF (NOT -1 (s1) OR (s2)) ;
    24	REMOVE Verb IF (NOT 1 (s0) OR (s2)) ;
    25	
    26	# Remove states between certain tags
    27	REMOVE (s0) IF (NOT 1 Det) (NOT 0 <<<) ;
    28	REMOVE (s0) IF (NOT -1 Verb) ;
    29	REMOVE (s1) IF (NOT 1 Verb OR Noun) ;
    30	REMOVE (s1) IF (NOT -1 Det OR Adj OR Noun) ;
    31	REMOVE (s2) IF (NOT 1 Verb OR Det OR Adj OR Noun) (NOT 0 <<<) ;
    32	REMOVE (s2) IF (NOT -1 Noun OR Verb) ;
```

Then we apply it to a symbolic sentence, where every word cohort has initially all tags, and every state cohort has all states.

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

Running with a trace, we can see which rules have removed which readings:

```
$ ./runExample.sh random 2 -t
"<s>"
	"s0" s0
;	"s1" s1 REMOVE:11
;	"s2" s2 REMOVE:11
"<w>"
	"det" det
;	"adj" adj REMOVE:19
;	"noun" noun REMOVE:21
;	"verb" verb REMOVE:23
"<s>"
	"s1" s1
;	"s0" s0 REMOVE:27
;	"s2" s2 REMOVE:32
"<w>"
	"verb" verb
	"noun" noun
;	"det" det REMOVE:18
;	"adj" adj REMOVE:20
"<s>"
	"s0" s0
	"s2" s2
;	"s1" s1 REMOVE:12
```
