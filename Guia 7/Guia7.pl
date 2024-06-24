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


% Ejercicio 7

%i. intersección(+L1, +L2, -L3)


%interseccionAux([L1H|L1T],L2,L3,V):- member(L1H,L2), member(L1H,L3) , not(member(L1H,V)) , interseccionAux (L1H,L2,L3,[L3H|V]).
%interseccion(L1,L2,L3):-interseccionAux(L1,L2,L3,[]).


% Ejercicio 12

% bin(izq, v, der),
vacio(nil).

raiz(bin(_, V, _),V).


altura(nil,0).
altura(bin(IZQ,_,DER),N) :- altura(IZQ,NI) , altura(DER,ND) , N is max(NI,ND) + 1 .
	
cantidadDeNodos(nil,0).
cantidadDeNodos(bin(IZQ,_,DER),N):- cantidadDeNodos(IZQ,NI) , cantidadDeNodos(DER,ND) , N is NI + ND + 1.

% ejemplo : funcion(bin(bin(bin(nil,2,nil),4,nil),5,bin(nil,6,nil)),X).

% Ejercicio 13

% inorder(+AB,-Lista)

inorder(nil,[]).
inorder(bin(IZQ,R,DER),L) :- inorder(IZQ,LI) , inorder(DER, LR) , append(LI,[R],L1) , append(L1,LR,L).

/*
% arbolConInorder(+Lista,-AB)
arbolConInorder([], nil).
arbolConInorder([LH|LT], bin(IZQ,R,DER)) :- ground(R),arbolConInorder([LH|LT],IZQ).
arbolConInorder([LH|LT], bin(IZQ,LH,DER)) :- arbolConInorder(LT,DER).
%arbolConInorder([LH|LT], bin(IZQ,R,DER)) :- member(R,LT),arbolConInorder([LH|LT],IZQ).


arbolConInorder([], nil).
arbolConInorder(LH, bin(nil,LH,nil)).
arbolConInorder([LH|LT], bin(nil,LH,DER)) :-arbolConInorder(LT,DER).
arbolConInorder([LH|LT], bin(IZQ,R,nil)) :- arbolConInorder([LH|LT],IZQ).
arbolConInorder([LH|LT], bin(IZQ,LH,DER)) :- arbolConInorder(LT,IZQ) .
arbolConInorder([LH|LT], bin(IZQ,R,DER)) :- arbolConInorder([LH|LT],DER) .
*/
% ejemplo : arbolConInorder([2, 4, 5, 6],X).

% aBB(+T)


aBB(nil).
aBB(bin(nil,_,nil)).
aBB(bin(IZQ,R,nil)) :- raiz(IZQ,RI) , R >= RI , aBB(IZQ).
aBB(bin(nil,R,DER)):- raiz(DER,RD) , R =< RD , aBB(DER).
aBB(bin(IZQ,R,DER)):- raiz(IZQ,RI) , raiz(DER,RD) , R >= RI , R =< RD , aBB(IZQ) , aBB(DER).

% ejemplo aBB(bin(bin(bin(nil,2,bin(nil,3,nil)),4,nil),5,bin(nil,6,nil))).

% aBBInsertar(+X, +T1, -T2)
aBBInsertar(X,nil,bin(nil,X,nil)).
%aBBInsertar(X,bin(IZQ,X,DER),bin(IZQ,X,DER)). % por si son iguales , aunque no hace falta
aBBInsertar(X,bin(IZQ,R,DER),bin(IZQ2,R,DER)) :- X < R , aBBInsertar(X,IZQ,IZQ2).
aBBInsertar(X,bin(IZQ,R,DER),bin(IZQ,R,DER2)) :- X > R , aBBInsertar(X,DER,DER2).

% Ejercicio 15

desde2(X,X).
desde2(X,Y) :- var(Y), N is X+1, desde2(N,Y).
desde2(X,Y) :- nonvar(Y), X < Y.
todasLasFilasSumanLoMismo(XS) :- not((member(E1,XS), member(E2,XS) , E1 \= E2 , sumlist(E1,N1) , sumlist(E2,N2) , N1 \= N2 )).

cuadradoLat(N,M):- desde2(0,P), matrices(N,P,N,M).

matrices(_,_,0,[]).
matrices(N,P,C,[L|M]):- C > 0, generarLista(N,P,L) ,Y is C-1, matrices(N,P,Y,M).

generarLista(0,0 ,[]).
generarLista(N,P ,[X|XS]) :-
    N > 0, P >= 0,
    between(0, P, X),
    R is P - X, Y is N-1,
    generarLista(Y,R,XS).

%ahora con generate and test.
generarLista1(0,_ ,[]).
generarLista1(N,P ,[X|XS]) :-
    N > 0, P >= 0,
    between(0, P, X),
     Y is N-1,
    generarLista1(Y,P,XS).

cuadradoLat1(0,[]).
cuadradoLat1(N,M):- desde2(0,P), matrices1(N,P,N,M) , todasLasFilasSumanLoMismo(M).

matrices1(_,_,0,[]).
matrices1(N,P,C,[L|M]):- C > 0, generarLista1(N,P,L),Y is C-1, matrices1(N,P,Y,M).