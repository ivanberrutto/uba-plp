%% last(?L, ?U), donde U es el último elemento de la lista L
last([ X ], X).
last((_|T),Y) :- last(T,Y).

%mismaLongitud (+L,+L1)
mismaLongitud(L,L1) :- length(L,N) , length(L1,N).

%% reverse(+L, -L1), donde L1 contiene los mismos elementos que L, pero en orden inverso.
reverse([],[]).
reverse([X|XS],Y) :- mismaLongitud([X|XS],Y), reverse(XS,Z), append(Z, [X] , Y).


%% maxlista(+L , -M)  donde M es el maximo de cada lista.

maxlista([ X ] , X) .
maxlista([X,XS], X) :- maxlista(XS,Z), Z =< X.
maxlista([X,XS], Y) :- maxlista(XS,Y), X =< Y.

%% prefijo(?P , +L) donde S es el prefijo de la lista L
%prefijo([],[_]).
%prefijo([X],[X]).
prefijo([X],[X|_]).
prefijo([X|XS],[Y|YS]) :- prefijo(XS,YS) , X=Y.

%% sufijo(?S, +L), donde S es sufijo de la lista L.
%sufijo([],[_]).
%sufijo([X],[X]).
sufijo(X,Y) :- reverse(Y,Z), prefijo(Z2,Z) , reverse(Z2,X) .

%sublista(?S, +L), donde S es sublista de L.
sublista(X,Y) :- append(Z,_,Y) , append(_,X,Z) .

%pertenece(?X, +L), que es verdadero sii el elemento X se encuentra en la lista L. (Este predicado ya vienedefinido en Prolog y se llama member).
%pertenece(X,[X]).
pertenece(X,[X|_]).
pertenece(Y,[_|XS]) :- pertenece(Y,XS).

