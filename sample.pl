father(fred,pebbles).
father(bamm-bamm,roxy).
father(barney,bamm-bamm).
father(bamm-bamm,chip).

mother(pebbles,roxy).
mother(pebbles,chip).
mother(wilma,pebbles).
mother(betty,bamm-bamm).

grandmother(X,Y) :- mother(X,Z), parent(Z,Y).

parent(X,Y) :- mother(X,Y).
parent(X,Y) :- father(X,Y).

