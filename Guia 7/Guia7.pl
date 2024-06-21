%% Ejercicio 1

padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y). 
hijo(X,Y) :- padre(Y,X).
hermano(X,Y) :- padre(Z,X) , padre(Z,Y).
descendiente(X,Y) :- hijo(X,Y).
descendiente(X,Y) :- hijo(X,Z) , descendiente(Z,Y) .


%% Ejercicio 4

% juntar(?Lista1,?Lista2,?Lista3)
juntar([],Lista2,Lista2).
juntar([X|T1],Lista2,[X|T3]) :- juntar(T1,Lista2,T3).

%% Ejercicio 5

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


% Ejercicio 6

aplanar([],[]).
aplanar([ [] | T ], Res) :-
	aplanar(T, Res).
aplanar([ [X | T1 ] | T ], Res) :-
	aplanar([ X | T1 ], Y),
	aplanar(T, RecT),
	append(Y, RecT, Res).
aplanar([ X | T ], [X | Res]) :-
	not(is_list(X)),
	aplanar(T, Res).


