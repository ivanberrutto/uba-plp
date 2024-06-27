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
caminoValido(F,F,_,_,[]).
caminoValido(I,F,T,V,[P|C]):- not(I=F),vecinoLibre(I,T,P),not(member(P,V)),caminoValido(P,F,T,[P|V],C).
%camino(I,I,_,[]).
camino(I,F,T,[I|C]):- caminoValido(I,F,T,[I],C).

%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

% Analizando la reversibilidad de F, F al no venir instanciada unificara con la primera linea de camino valido, 
% pero al solicitar otra solución en la siguiente linea ya no unificará, 
% ya que "not(_ = F)" retornara siempre False. Por lo tanto, F no es reversible

%  En el caso de C, si esta instanciado, va a dar True si cada elemento efectivamente cumple con las condiciones que cada posicion del
%  tiene que cumplir en un camino valido, y que empieza y termine donde lo tiene que cumplir. Caso contrario da false
%  Por lo tanto C es reversible

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

% desdeHasta(+Tablero , -Largo)
largoTablero([TH|TT],L) :- length([TH|TT],F) , length(TH,C) , L is F*C.

% desdeMenoresCaminos(+Tablero, -Camino)
desdeMenoresCaminos(T,C) :- largoTablero(T,F),
                            between(0,F,L),
                            length(C,L).

%camino2(I,I,_,[]).
camino2(I,F,T,[I|C]):- desdeMenoresCaminos(T,[I|C]), caminoValido(I,F,T,[I],C).

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.

% Cuando I si viene instanciada, ocurre un caso parecido al de F en camino, ya que unifica en el caso donde I es igual que F, 
% pero luego al pedir la siguiente solucion da false por el "not(_ = I)". Entonces, I no es reversible.

% En C tambien es analogo al C de 5.1 , ya que devuelve True si cada elemento cumple las condiciones. En caso contrario da false.
% Por lo tanto C es reversible

%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.

caminoOptimo(I,F,T,C):- camino(I,F,T,C), not((camino(I,F,T,C1) ,length(C,A),length(C1,B),B<A)).

%% En caminoOptimo utilizamos estrategia Generate and Test, ya que instanciamos todos los caminos posibles 
%% y luego podemos verificar que sean uno de los caminos con la menor longitud de pasos posible.

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
tablero(ejpos0Tapada,T) :- tablero(5, 5, T),ocupar(pos(1, 0), T),ocupar(pos(0, 1), T),ocupar(pos(1, 1), T).

%Para probar con ej5x5 en caminoDual
tablero(ejunasol, T) :- tablero(5, 5, T),ocupar(pos(0, 2), T),ocupar(pos(2,1), T),ocupar(pos(3,1), T),ocupar(pos(3,2), T),ocupar(pos(3,3), T).
tablero(ejsinsol, T) :- tablero(5, 5, T),ocupar(pos(0, 2), T),ocupar(pos(1,0),T).
tablero(ejcuatrosol, T) :- tablero(5, 5, T),ocupar(pos(0, 2), T),ocupar(pos(2,2), T),ocupar(pos(3,2), T),ocupar(pos(3,3), T).


cantidadTestsTablero(4). % Actualizar con la cantidad de tests que entreguen
% Caso ejemplo inicial
testTablero(1) :- tablero(0,0,[]).
testTablero(2) :- ocupar(pos(0,0), [[ocupada]]).
% Caso Tablero 3x3
testTablero(3) :- tablero(3,3,[[_,_,_],[_,_,_],[_,_,_]]). 
% Caso tablero con pos 2x2 ocupada.
testTablero(4) :- ocupar(pos(2,2),[_, _, [_, _, ocupada ,_],_] ).


cantidadTestsVecino(4). % Actualizar con la cantidad de tests que entreguen
% Caso ejemplo inicial
testVecino(1) :- vecino(pos(0,0), [[_,_]], pos(0,1)).
% Caso solo un vecino ocupado
testVecino(2) :- vecino(pos(1,0), [[_,_,ocupada],[_,_,ocupada],[_,_,_]] , _ ). 
% Caso rodeado de ocupados
testVecino(3) :- not(vecinoLibre(pos(1,1),[[_,ocupada],[ocupada,_],[_,ocupada]],_)). 
% Caso solo un vecino libre
testVecino(4) :- vecinoLibre(pos(1,1),[[_,ocupada],[_,_],[_,ocupada]],_). 


cantidadTestsCamino(7). % Actualizar con la cantidad de tests que entreguen
% Ejemplo inicial
testCamino(1) :- tablero(ej5x5, T), camino2(pos(0,0), pos(2,3), T, _).
% Test de camino con Inicio fuera del tablero
testCamino(2) :- not( (tablero(ej5x5, T), camino2(pos(8,0), pos(2,3), T,_)) ).
% Test de camino con Inicio tapado
testCamino(3) :- not( (tablero(ejpos0Tapada, T), camino2(pos(0,0),pos(2,3), T,_)) ).
% Test de camino con Final tapado
testCamino(4) :- not( (tablero(ejpos0Tapada, T), camino2(pos(2,3),pos(0,0), T,_)) ).
% Test Reversibilidad de C con camino valido
testCamino(5):-tablero(ej5x5, T), camino2(pos(0,0), pos(2,3), T ,[pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2), pos(2, 3)]).
% Test Reversibilidad de C con camino que no comienza en Inicio.
testCamino(6):- not( (tablero(ej5x5, T), camino2(pos(0,0), pos(2,3), T,[pos(0, 1), pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2,2),pos(2, 3)])) ).
% Test Reverisibilidad de C con camino que pasa por ocupada.
testCamino(7):-not( (tablero(ej5x5, T), camino2(pos(0,0), pos(2,3), T,[pos(0, 0), pos(1, 0), pos(1, 1), pos(2, 1), pos(2, 2), pos(2, 3)])) ).



cantidadTestsCaminoOptimo(3). % Actualizar con la cantidad de tests que entreguen
% Test de dos caminos optimos
testCaminoOptimo(1) :- tablero(ej5x5, T), caminoOptimo(pos(0,0), pos(2,3), T, _).
% Test un camino optimo recto
testCaminoOptimo(2) :- tablero(ej5x5, T), caminoOptimo(pos(0,0), pos(4,0), T, _).
% Test sin camino optimo
testCaminoOptimo(3) :- not(( tablero(ejpos0Tapada, T), caminoOptimo(pos(4,0), pos(0,0), T, _) )).

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

