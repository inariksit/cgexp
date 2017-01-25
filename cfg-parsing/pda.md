```
<stack>
    Z


<s>
    p
    pq
	q
    qr
	r
<w>
    a
    b
<s>
    p
    pq
    q
    qr
    r
<w>
    a
    b
<s>
    p
    pq
    q
    qr
    r
<w>
    a
    b
<s>
    p
    pq
    q
    qr
    r
<w>
    a
    b
<s>
    p
    pq
    q
    qr
    r
```

The states `pq` and `qr` mean there's an ε-transition from `p` to `q` or `q` to `r`.

BEFORE-SECTIONS

REMOVE q OR r IF (0 >>>) ;
REMOVE p OR pq IF (1 <<<) ;

SECTION

# Read a, move in p, push A to stack
REMOVE b IF (-1C p) (-2C* @Z) ;
"<stack>" MAP @AZ ;

# Read b, move in q, pop A from stack
REMOVE a IF (NOT 1 p) ;
"<stack>" REMOVE A ;


REMOVE q OR r OR qr IF (-1C a) ;
REMOVE p OR r OR qr IF (1C b) ;

SECTION
# Read 0, move from p to q, keep stack as is
# Read 0, move from q to r, stack must be Z.

wikipedia: "In a sense the stack of the PDA contains the unprocessed data of the grammar, corresponding to a pre-order traversal of a derivation tree."


AFTER-SECTIONS

REMCOHORT (*) IF (NOT -1C* Z) ;
REMCOHORT (*) IF (-1* a LINK 0 b) ;




grammar:
  S -> aSb
  S -> ab


Order of execution in VISL CG-3

        ForEach (Window)
            ForEach (Rule)
             ForEach (Cohort)
              ApplyRule