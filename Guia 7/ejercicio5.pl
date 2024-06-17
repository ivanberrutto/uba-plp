%% last(?L, ?U), donde U es el último elemento de la lista L
last([ X ], X).
last((_|T),Y) :- last(T,Y).

%mismaLongitud (+L,+L1)
mismaLongitud(L,L1) :- length(L,N) , length(L1,N).

%% reverse(+L, -L1), donde L1 contiene los mismos elementos que L, pero en orden inverso.
reverse([],[]).
reverse([X|XS],Y) :- mismaLongitud([X|XS],Y), reverse(XS,Z), append(Z, [X] , Y).


%% maxlista(+L , -M) , minlista(+L, -M) donde M es el maximo/minimo de cada lista.

maxlista([ X ] , X) .
maxlista([X,XS], X) :- maxlista(XS,Z), Z =< X.
maxlista([X,XS], Y) :- maxlista(XS,Y), X =< Y.

