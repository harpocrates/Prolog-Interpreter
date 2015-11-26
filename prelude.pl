unify(X,X).


list([]).
list(cons(X,Xs)) :- list(Xs).

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

member(X,Ys) :- append(Ws,[X|Zs],Ys).


number(0).
number(s(N)) :- number(N).

plus(0,Y,Y).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

times(0,Y,0).
times(s(X),Y,Z) :- times(X,Y,W), plus(Y,W,Z).

exponent(X,0,1).
exponent(X,s(Y),Z) :- exponent(X,Y,W), times(X,W,Z).

greater(s(X),0).
greater(s(X),s(Y)) :- greater(X,Y).

equal(0,0).
equal(s(X),s(Y)) :- equal(X,Y).

gcd(X,X,X).
gcd(X,Y,G) :- greater(Y,X), plus(Y1,X,Y), gcd(X,Y1,G).
gcd(X,Y,G) :- greater(X,Y), gcd(Y,X,G).
