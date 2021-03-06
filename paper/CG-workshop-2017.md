# Exploring the Expressivity of Constraint Grammar

## Introduction

Traditionally, CG is seen as a practical and language-oriented approach to NLP; it is a *tool* rather than a *formalism*, *framework* or anything of the sort. Since the beginning of CG \cite{karlsson1995cg}, its authors do not envision CG being capable of generation, only for analysis. 
Despite its decidedly linguistic roots, we argue that CG is interesting to look at also from a computational perspective. For the theoretically inclined, making a description of a formalism more precise is an end of its own. However, even a practically oriented reader should appreciate the applications: better understanding of a formalism may lead to novel uses and implementation techniques.
In this paper, we address the question of the *expressivity* of CG. We do not reach a definite answer, but hope to create new ways of reason about reductionistic grammar formalisms.


## Background and previous work

The standard measure of formal languages is the Chomsky hierarchy, with its four
classes of grammars and languages, in order from most expressive to least expressive:
recursively enumerable (Type 0), context-sensitive (Type 1), context-free (Type 2), and
regular (Type 3).
The notion of expressive power, “which constructs can we express in the language”, is coupled with parsing complexity, “how much time and memory do we need to parse sentences in the language”; more expressive power corresponds to greater parsing complexity.

The expressivity of a single rule is easy to define. Consider the contextual tests of a `REMOVE` or `SELECT` rule, such as `IF (NOT 1* Verb OR Noun)`: just seeing this condition hints that we can express a subset of regular languages that contains at least disjunction, complement and Kleene star. \cite{tapanainen1999phd} gives a precise definition of the expressivity of a single rule for 4 different constraint formalisms. 
However, the expressivity of the whole grammar is harder to define. 
A given grammar in CG does not describe a language, but a *relation* between an input language and an output language.
In addition, we need to consider a number of implementation features: there are many
variants of CG \cite{mostRelevantCGs}, with a different take on rule ordering, execution strategy, and supported operations. 

In contrast, parsing complexity can be easily defined for a given variant and implementation of CG. For instance, \cite{tapanainen1999phd} and \cite{hulden2011cg_engine} analyse their CG-2 parser implementations 
in terms of the sentence length $n$; 
the number of rules in the grammar $G$; and the maximal number of different readings per cohort $k$. Tapanainen's parser has the worst-case complexity $O(Gn^3k^2)$, and Huldén's parser $O(Gn^2k^2)$;
even though both systems can run the same CG-2 style grammars, the complexities of the respective parsers are different, due to different implementation techniques. 




## Our approach

> We view a constraint grammar as a formal language L\mathcal{L}L, generated over an 
> alphabet Σ\SigmaΣ. We generate the strings in our language by passing 
> *maximally ambiguous strings* of every length to the grammar. 
> With maximally ambiguous, we mean  those strings where each position contains the 
> entire alphabet, so ⟨Σ⟩n\langle \Sigma \rangle_n⟨Σ⟩​n​​. 
> A constraint grammar is said to accept a string *w* of length *n* if, 
> when we pass ⟨Σ⟩n\langle \Sigma \rangle_n⟨Σ⟩​n​​ as an input to the CG,
> *w* is one of the possible interpretations of its output.

**TODO: define the subset of CG we use, and mean that when we say "CG" from now on.**


### Is CG regular?

In order to claim that a formal language belongs to a certain class, we need to show that it can express *all* constructs in the said class. For instance, if CG can express the language `a*b*`, it is not yet evidence that CG is "regular": for all we know, CG could just have a common subset with regular languages, illustrated in Figure X:

```
 (figure X)
       ______
  ____/      \
 /   /\ Reg.  \ 
| CG|  | lang |
|   |  |      |
 \___\/      /
      \_____/
```


Or it could be like in Figure Y: encompassing all of the regular languages, and some from context-free and context-sensitive, but not all.

``` ____
  _/ CG \_____
 /  _____     \
|  / Reg. \    |
| | lang  |    |
|  \_____/     |
 \____        / 
      \______/
```

We know that all regular languages can be expressed by finite automata. Thus, in order to answer "is CG regular?", we can try to find a conversion from any arbitrary automaton to a corresponding CG.


#### Deriving CGs from REs

This is how we can (maybe???) do it: [link](https://github.com/inariksit/cgexp/blob/master/README.md)

#### A note about disjunction 

One obvious flaw is the lack of disjunction in CG. Consider the regular expression `ab|ba`, accepting the language `{ab,ba}`.
Given a maximally ambiguous sentence of size 2, this is the closest we could get in CG output:

```
"<w>" 
  	a
  	b
"<w>" 
  	a
  	b
```	

If our alphabet consists of anything more than `{a,b}`, all the other readings would correctly be removed. But CG cannot express the disjunction any further: the description "either *ab* or *ba*" is translated into a coarser description: "the first character may be either *a* or *b*, and the second character may be either *a* or *b*".

Another option would be to return a set of CGs: one that disambiguates into *ab* and other that disambiguates into *ba*. For this example, we could force just the first item into either *a* or *b*, by adding a rule `SELECT a IF (-1 >>>)` or `SELECT b IF (-1 >>>)`, and then some more C conditions will match, hence more rules will apply.

But probably there are more complex REs where just fixing the first character is not enough: e.g. `(aab|b)*(ab|ba)`, if we fix `a`, it can be the first `a` of `aab` or `ab`

Another vague idea which is probably wrong: expand all the words in the language (up to some depth), and write constraint rules that will only select those words, nothing more. There's no C, so the rules start disambiguating right away. 


| Accepts *ab*             | . | Accepts *ba*             |
|--------------------------|---|--------------------------|
| `SELECT (b) IF (-1 a) ;` | . | `SELECT (a) IF (-1 b) ;` |
| `SELECT (a) IF ( 1 b) ;` | . | `SELECT (b) IF ( 1 a) ;` |


### CG is beyond regular: the language aⁿbⁿ

**Assuming that the RE->CG conversion works, otherwise scrap this:** It took a lot of work to show that CG *is* at least regular. Now, can we go further? Can we transform all context-free grammars into CGs?

Unfortunately, transforming CFGs into CGs is not as easy as transforming automata. We relied on the fact that any symbol comes directly after the state it transitions from, there is no memory. But the structure of a CFG is nested, and CG is very linear, no hidden nodes (except dependency in CG3 but that isn't part of our fragment!).

To reveal the ending, we don't have a solution--it may or may not exist. But we can show a hand-crafted grammar, which recognises a solidly context-free language `aⁿbⁿ` for arbitrary n. Just this one counterexample proves that CG is *beyond* regular. No guarantees how much of an overlap there is with CG and context-free, just that it is beyond regular.

**TODO: explanation from Pepijn's blog post**

### CG is beyond context-free: the language aⁿbⁿcⁿ

Can we go further? Yes! By same reasoning, we can show that CG extends at least some parts outside context-free, by providing a grammar that accepts the language `aⁿbⁿcⁿ` for an arbitrary n.

**TODO: explanation from Pepijn's blog post**

## Discussion

### Practical benefits: derive CGs from perhaps more easily defined formalisms?

CG can act as a preprocessing step for some more expensive parser. Then the rules can be non-human-oriented, and contain extra symbols. The rules would be derived from the grammar itself, with the sole purpose of making the parsing *faster*, not more accurate.


#### Using templates

We may not need to aim for complete coverage, but just some useful rules. (Though I'm pretty sure these are going to be super trivial, and as easy to come up with in CG as in writing a CFG.)

```
S -> NP VP
NP -> Det N | MassN | Predet NP
VP -> V NP | V
N -> bear | ...
V -> bear | ...
```

Some things are easy to extract: `SELECT N IF (-1C Det)`, assuming that there is nowhere a sequence `Det something-not-N`. 
Many rules have something more complex on the RHS. But we can also access those in the conditions, by using *templates*. They are also recursive, so we can do this:

```
TEMPLATE NP = (1 Det LINK 1 N) OR 
					    (1 MassN) OR
					       (1 Predet LINK 1 T:NP) ;
```

We can use templates in the conditional tests just like any other lists or sets:


```
REMOVE V IF (-1 Det) ;

SELECT V IF (-1 T:NP) ;
```

Easy (and probably useless) derivation: `SELECT/REMOVE <any category that expands to a terminal> IF (<position>C T:<any category that doesn't expand to a terminal>)`.


#### Using a parser

How does a CFG resolve lexical ambiguities? Initially some word has >1 productions where it's on the RHS; but when the parser is considering the word in its context, only one of those productions can fit into the greater structure, that matches the whole sentence. 
But if we think like a CG grammarian, we don't want all the fancy structure. We just want to know: **after reading which word(s) did the parser know to discard the irrelevant analyses of the ambiguous word**? (It is not always the previous one. Take any chart parser, and look at the intermediate results.)



## Conclusion

Thanks for reading, hope it made sense!
