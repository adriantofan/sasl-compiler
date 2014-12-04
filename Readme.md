#a SASL compiler in Haskell

An implementation of a SASL interpreter in Haskell. This repository came to life as an exercise and a followup to this [tweet][MeijerSASL] from Erick Meijer. 

I am grateful to him and to all those(see ) who laid such a clear and effective path to learning. 

## Disclaimer

* It is a working implementation with some serious limits(see bellow).
* It is absolutely not a model of Haskell programming style.
* It is my first Haskell program, first Haskell debugging session, first time use of monads and monad transformers, first parser implementation, first contact with SASL, first contact with a SK reduction machine & lambda calculus
* It(still) might be useful for somebody else who follows the same path in learning. It took me two and a half weeks and less than 750 lines of code. It was a **very** steep dive and the ultimate Haskell tutorial. 


## Examples

Map function:

```
$ echo "\
map f [1,2,3] where
  map g x = if x = nil then nil else g (hd x) : map g (tl x)
  f x = x * x" | runhaskell Main.hs
result :
(S @ (S @ (K @ S) @ (S @ (S @ (K @ S) @ (S @ (K @ K) @ (K @ S))) @ (S @ (S @ (K @ S) @ (S @ (S @ (K @ S) @ (S @ (K @ K) @ (K @ S))) @ (S @ (S @ (K @ S) @ (S @ (K @ K) @ (K @ K))) @ (S @ (K @ K) @ (K @ S))))) @ (S @ (S @ (K @ S) @ (S @ (S @ (K @ S) @ (S @ (K @ K) @ (K @ S))) @ (S @ (S @ (K @ S) @ (S @ (K @ K) @ (K @ K))) @ (S @ (K @ K) @ (K @ K))))) @ (S @ (K @ K) @  ............................................................................................... and  69000 more  characters.
```

Same Map , only showing head on a different list:

```
$ echo "\
hd(map f [3,2,1]) where
  map g x = if x = nil then nil else g (hd x) : map g (tl x)
  f x = x * x" | runhaskell Main.hs
Result:
Num 9
```


## What's in it

  - ParserST.hs - a re-implementation of monad transformers (State, Reader) and the Parser Type
  - Grammar.hs - low level parsing utility based on [Monadic Parser Combinators][monparsing]
  - SASL.hs - SASL parser  (lexer-parser fronted)
  - SK.hs - the SK reduction machine (compiler-reduction backend)
  - Main.hs - high level utilities and command line interface

The backend uses only a couple of math and boolean Haskell built-in operators. All the rest is handled inside the reduction machine.

## Limits

  * where expressions support only maximum two mutually-recursive sub expressions
  * global defs are partially implemented and NOT used 
  * error reporting is minimal, probably not very helpful
  * no optimizations whatsoever 
  * ; separator not used - relies on indentation

## Next steps

Not sure if there is a next, but I would love to work on fixing some of the limits.


## References:

  * [Graham Hutton, Erik Meijer, 1996,Monadic Parser Combinators][monparsing]
  * [Torsten Grust, 1997,The construction of a SASL compiler][preprint-043]
  * [D. A. Turner,1979, A new implementation technique for applicative languages][turner-implementation]
  * [D. A. Turner,1976, SASL Language Manual][saslman]
  * [Erik Meijer #FP101x SASL tweet][MeijerSASL]
  * [D.A. Turner, 1981, The semantic elegance of applicative languages][paraffins-turner]
  * [D.A.Turner, 1985, Miranda: A non-strict functional language with polymorphic types][nancypaper]
  * [David Turner, 2006(?), Church’s Thesis and Functional Programming][ctfp]
  * [Philip Wadler 1992, The essence of functional programming][wadler92essence]
  * [Bartosz Milewski's, Monads for the Curious Programmer][monads-for-the-curious-programmer]
  
  

[monparsing]:http://www.cs.nott.ac.uk/~gmh/monparsing.pdf
[saslman]:http://www.eis.mdx.ac.uk/staffpages/dat/saslman.pdf "D A Turner,1976, SASL Language Manual"
[MeijerSASL]:https://twitter.com/headinthebox/status/533262396866191360 "Erik Meijer #FP101x SASL tweet"
[turner-implementation]:https://courses.engr.illinois.edu/cs421/sp2012/project/turner-implementation.pdf
[paraffins-turner]:http://nsl.com/misc/sasl/paraffins-turner.pdf
[ctfp]:http://www.eis.mdx.ac.uk/staffpages/dat/ctfp.pdf
[preprint-043]:http://www.inf.uni-konstanz.de/Preprints/papers/1997/preprint-043.pdf
[nancypaper]:http://www.cs.kent.ac.uk/people/staff/dat/miranda/nancypaper.pdf
[wadler92essence]:http://www.eliza.ch/doc/wadler92essence_of_FP.pdf
[monads-for-the-curious-programmer]:http://bartoszmilewski.com/2011/01/09/monads-for-the-curious-programmer-part-1