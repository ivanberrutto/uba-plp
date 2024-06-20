%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

%% Ejemplo para usar 
tablero(ej5x5, T) :- tablero(5, 5, T),ocupar(pos(1, 1), T),ocupar(pos(1, 2), T).
tablero(libre20, T) :- tablero(20, 20, T).


lista(0,[]).
lista(N,[X|XS]):- length([X|XS], N), lista(Z ,XS) , X= _ , Z is N-1 .

tablero(0,_,[]).
tablero(F,C,[X|XS]):- length([X|XS],F) , tablero(F1 , C , XS) , X = X1 , lista(C,X1) , F1 is F-1 .




%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.

elementoTablero(pos(0,0),[[X|_]|_],X).
elementoTablero(pos(0,C),[[_|XS]|YS],E):- 0 \= C , C1 is C-1 , elementoTablero(pos(0,C1),[XS|YS],E) .
elementoTablero(pos(F,C),[_|XS],E):- 0 \= F ,  F1 is F-1 , elementoTablero(pos(F1,C),XS,E).

ocupar(pos(F,C),T) :- elementoTablero(pos(F,C),T,ocupada).
/*
ocupar(pos(0,0),[[ocupada|_]|_]) .
ocupar(pos(0,C),[[_|XS]|YS]):- 0 \= C , C1 is C-1 , ocupar(pos(0,C1),[XS|YS]) .
ocupar(pos(F,C),[_|XS]):- 0 \= F ,  F1 is F-1 , ocupar(pos(F1,C),XS).
*/



%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

%between(I,F,N).
%head([H|_], H).


vecino(pos(X,Y),[T|Ts],pos(F,Y)):- F is X+1, length([T|Ts],P), between(0,P,F).
vecino(pos(X,Y),[T|Ts],pos(F,Y)):- F is X-1, length([T|Ts],P), between(0,P,F).
vecino(pos(X,Y),[T|_],pos(X,J)):- J is Y+1, length(T,P), between(0,P,J).
vecino(pos(X,Y),[T|_],pos(X,J)):- J is Y-1, length(T,P), between(0,P,J).



%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero

vecinoLibre(pos(X,Y),[T|Ts],V):- vecino(pos(X,Y),[T|Ts],V) , elementoTablero(V,[T|Ts],E),var(E).
%vecinoLibre(pos(X,Y),[T|Ts],pos(X,J)):- vecino(pos(X,Y),[T|Ts],pos(X,J)) , elementoTablero(pos(X,J),[T|Ts],E),var(E).

/*
vecinoLibre(pos(X,Y),[T|Ts],pos(F,Y)):- F is X+1, length([T|Ts],P), between(0,P,F) , elementoTablero(pos(F,Y),[T|Ts],E),var(E).
vecinoLibre(pos(X,Y),[T|Ts],pos(F,Y)):- F is X-1, length([T|Ts],P), between(0,P,F) , elementoTablero(pos(F,Y),[T|Ts],E),var(E).
vecinoLibre(pos(X,Y),[T|Ts],pos(X,J)):- J is Y-1, length(T,P), between(0,P,J) , elementoTablero(pos(X,J),[T|Ts],E),var(E).
vecinoLibre(pos(X,Y),[T|Ts],pos(X,J)):- J is Y+1, length(T,P), between(0,P,J) , elementoTablero(pos(X,J),[T|Ts],E),var(E).
*/


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




/* comento todo 

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace



%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.
camino2(_,_,_,_).

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.


%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
caminoOptimo(_,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
caminoDual(_,_,_,_,_).

%%%%%%%%
%% TESTS
%%%%%%%%

cantidadTestsTablero(2). % Actualizar con la cantidad de tests que entreguen
testTablero(1) :- tablero(0,0,[]).
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
% Agregar más tests

cantidadTestsVecino(1). % Actualizar con la cantidad de tests que entreguen
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
% Agregar más tests

cantidadTestsCamino(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsCaminoOptimo(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsCaminoDual(0). % Actualizar con la cantidad de tests que entreguen
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

*/