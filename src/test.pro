child(copil1).
child(copil2).

female(f).
male(m).

parent(m, copil1).
parent(m, copil2).

parent(f, copil1).
parent(f, copil2).

father(X,Y):-male(X),parent(X,Y).
mom(X,Y):-female(X),parent(X,Y).

test(X,Y,Z):-father(X,Y),known(Z).

known(k1).
known(k2).

