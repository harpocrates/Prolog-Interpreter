# Prolog-Interpreter

This is a no bells and whistles prolog implementation (for the moment). Extra-logical and meta-logical predicates are not defined (yes, there is no cut). There are two sugarings for numbers and lists available:

  * Numbers get translated into their Peano representations: `5` desugars to `s(s(s(s(s(zero)))))`.
  * Lists get translated into pairs: `[a,b,c]` desgugars to `cons(a,cons(b,cons(c,empty)))` and `[x1,x2|xs]` desugars to `cons(x1,cons(x2,xs))`.
  
The interpreter comes with a small prelude library of basic list/arithmetic operations that is automatically loaded. The arithmetic operations are *very* slow but with good reason: they are just regular predicates defined on Peano numbers.

# Sample usage

Build with `stack build` or `cabal build` and run with `stack exec -- prolog sample.pl` or `cabal run`. From just the prelude, we can query:

    > gcd(42,216,X)?
    { X = 6 };
    no.
    > append(X,Y,[a,b,c,d,e]), length(X,N), greater(N,2)?
    { X = [a,b,c], Y = [d,e], N = 3 };
    { X = [a,b,c,d], Y = [e], N = 4 };
    { X = [a,b,c,d,e], Y = [], N = 5 };
    no.
