%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

% lista(+Largo,-Lista)
lista(0,[]).
lista(N,[X|XS]):- length([X|XS], N), lista(Z ,XS) , X= _ , Z is N-1 .

tablero(0,_,[]).
tablero(F,C,[X|XS]):- length([X|XS],F) , tablero(F1 , C , XS) , X = X1 , lista(C,X1) , F1 is F-1 .


%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.

% elementoTablero(+Pos,?Tablero,?Elemento)
elementoTablero(pos(0,0),[[X|_]|_],X).
elementoTablero(pos(0,C),[[_|XS]|YS],E):- 0 \= C , C1 is C-1 , elementoTablero(pos(0,C1),[XS|YS],E) .
elementoTablero(pos(F,C),[_|XS],E):- 0 \= F ,  F1 is F-1 , elementoTablero(pos(F1,C),XS,E).

ocupar(pos(F,C),T) :- elementoTablero(pos(F,C),T,ocupada).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

vecino(pos(X,Y),[T|Ts],pos(F,Y)):- F is X+1, length([T|Ts],P), between(0,P,F).
vecino(pos(X,Y),[T|Ts],pos(F,Y)):- F is X-1, length([T|Ts],P), between(0,P,F).
vecino(pos(X,Y),[T|_],pos(X,J)):- J is Y+1, length(T,P), between(0,P,J).
vecino(pos(X,Y),[T|_],pos(X,J)):- J is Y-1, length(T,P), between(0,P,J).


%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero

vecinoLibre(pos(X,Y),[T|Ts],V) :- vecino(pos(X,Y),[T|Ts],V) , elementoTablero(V,[T|Ts],E),var(E).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas

% caminoValido(+Inicio,+Fin,+Tablero,+Visitados,-Camino)
caminoValido(F,F,_,V,[]):- length(V,X) , X = 1.
caminoValido(F,F,_,_,[]).
caminoValido(I,F,T,V,[P|C]):- not(I=F),vecinoLibre(I,T,P),not(member(P,V)),caminoValido(P,F,T,[P|V],C).
camino(I,I,_,[]).
camino(I,F,T,[I|C]):- caminoValido(I,F,T,[I],C).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

% ...

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

% Agregar longitud del camino para poder ordenar

% agregarlongitud(+Elem,-Largo-Elem)
agregarlongitud(Elem, L-Elem) :- 
    length(Elem, L).

% Recuperar camino original.
% quitarlongitud(+Largo-Elem,-Elem)
quitarlongitud(_-Elem, Elem).

% Ordeno las soluciones segun la cantidad de pasos que hicieron

% ordenarporlongitud(+Camino, -CaminoOrdenado)
ordenarporlongitud(Camino, CaminoOrdenado) :-

    maplist(agregarlongitud, Camino, CaminoConLongitud),
    sort(1, @=<, CaminoConLongitud, CaminoOrdenadoConLongitud),
    maplist(quitarlongitud, CaminoOrdenadoConLongitud, CaminoOrdenado).

camino2(I,I,_,[]).
% Consigo todas las soluciones, las ordeno por cantidad de pasos creciente y las voy dando.
camino2(I,F,T,[I|C]):- findall(X,caminoValido(I,F,T,[I],X),L), ordenarporlongitud(L,LS) , member(C,LS).

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.

% ...

%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.

caminoOptimo(I,F,T,C):- camino(I,F,T,C), not((camino(I,F,T,C1) ,length(C,A),length(C1,B),B<A)).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.

caminoDual(I, F , T1 , T2 , C):- camino(I,F,T1,C) , camino(I,F,T2,C).

%%%%%%%%
%% TESTS
%%%%%%%%

%% Tableros para usar
tablero(ej5x5, T) :- tablero(5, 5, T),ocupar(pos(1, 1), T),ocupar(pos(1, 2), T).
tablero(libre20, T) :- tablero(20, 20, T).
%Para probar con ej5x5 en caminoDual
tablero(ejunasol, T) :- tablero(5, 5, T),ocupar(pos(0, 2), T),ocupar(pos(2,1), T),ocupar(pos(3,1), T),ocupar(pos(3,2), T),ocupar(pos(3,3), T).
tablero(ejsinsol, T) :- tablero(5, 5, T),ocupar(pos(0, 2), T),ocupar(pos(1,0),T).
tablero(ejcuatrosol, T) :- tablero(5, 5, T),ocupar(pos(0, 2), T),ocupar(pos(2,2), T),ocupar(pos(3,2), T),ocupar(pos(3,3), T).



cantidadTestsTablero(2). % Actualizar con la cantidad de tests que entreguen
testTablero(1) :- tablero(0,0,[]).
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
% Agregar más tests

cantidadTestsVecino(1). % Actualizar con la cantidad de tests que entreguen
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
% Agregar más tests

cantidadTestsCamino(1). % Actualizar con la cantidad de tests que entreguen
testCamino(1) :- tablero(ej5x5, T), camino2(pos(0,0), pos(2,3), T, _).
% Agregar más tests

cantidadTestsCaminoOptimo(1). % Actualizar con la cantidad de tests que entreguen
testCaminoOptimo(1) :- tablero(ej5x5, T), caminoOptimo(pos(0,0), pos(2,3), T, _).

% Agregar más tests

cantidadTestsCaminoDual(3). % Actualizar con la cantidad de tests que entreguen

%Test un camino posible
testCaminoDual(1) :- tablero(ej5x5, T),tablero(ejunasol,T2), caminoDual(pos(0,0), pos(4,3), T,T2, _).
%Test camino sin solucion
testCaminoDual(2) :- not( ( tablero(ej5x5, T),tablero(ejsinsol,T2), caminoDual(pos(0,0), pos(4,3), T,T2, _) ) ).
%Test camino con cuatro soluciones
testCaminoDual(3) :- tablero(ej5x5, T),tablero(ejcuatrosol,T2), caminoDual(pos(0,0), pos(4,3), T,T2, _).

% Agregar más tests

tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).

tests(todos) :-
  tests(tablero),
  tests(vecino),
  tests(camino),
  tests(caminoOptimo),
  tests(caminoDual).

tests :- tests(todos).

