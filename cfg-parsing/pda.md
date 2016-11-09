```
<stack>
    Z


<s>
    p
	q
	r
<w>
    a
    b
<s>
    p
	q
	r
<w>
    a
    b
<s>
    p
	q
	r
<w>
    a
    b
<s>
    p
	q
	r
<w>
    a
    b
<s>
    p
	q
	r
```

BEFORE-SECTIONS

REMOVE q or R IF (0 >>>) ;
REMOVE p OR q IF (1 <<<) ;

SECTION

# Read a, move in p, push A to stack
# Read 0, move from p to q, keep stack as is
# Read b, move in q, pop A from stack
# Read 0, move from q to r, stack must be Z.


wikipedia: "In a sense the stack of the PDA contains the unprocessed data of the grammar, corresponding to a pre-order traversal of a derivation tree."


AFTER-SECTIONS

REMCOHORT (*) (NOT -1C* Z);




grammar:
  S -> aSb
  S -> ab


Order of execution in VISL CG-3

            ForEach (Window)
            ForEach (Rule)
             ForEach (Cohort)
              ApplyRule