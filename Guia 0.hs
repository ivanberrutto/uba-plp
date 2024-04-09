main = putStrLn "Hello World"
double x = 2 * x

-- Ejercicio 1
{- 
'null' tiene tipo [a] -> Bool.
Comprueba si una lista está vacía.

'head' tiene tipo [a] -> a.
Devuelve el primer elemento de una lista no vacía.

'tail' tiene tipo [a] -> [a].
Devuelve una lista sin el primer elemento.

'init' tiene tipo [a] -> [a].
Devuelve una lista sin el último elemento.

'last' tiene tipo [a] -> a.
Devuelve el último elemento de una lista no vacía.

'take' tiene tipo Int -> [a] -> [a].
Toma los primeros n elementos de una lista.

'drop' tiene tipo Int -> [a] -> [a].
Descarta los primeros n elementos de una lista.

'(++)' tiene tipo [a] -> [a] -> [a].
Concatena dos listas.

'concat' tiene tipo [[a]] -> [a].
Concatena una lista de listas en una sola lista.

'(!!)' tiene tipo [a] -> Int -> a.
Devuelve el elemento en la posición n de una lista (comenzando desde 0).

'elem' tiene tipo Eq a => a -> [a] -> Bool.
Verifica si un elemento está presente en una lista.
-}


-- Ejercicio 2

-- 'valorAbsoluto' toma un número de punto flotante y devuelve su valor absoluto.
valorAbsoluto :: Float -> Float
valorAbsoluto x
    | x >= 0    = x
    | otherwise = -x

-- 'bisiesto' toma un año representado como un número entero y devuelve True si el año es bisiesto, False en caso contrario.
bisiesto :: Int -> Bool
bisiesto year
    | (year `mod` 4 == 0 && year `mod` 100 /= 0) || year `mod` 400 == 0 = True
    | otherwise                                                         = False

-- 'factorial' calcula el factorial de un número entero positivo.
factorial :: Int -> Int
factorial n
    | n == 0    = 1
    | otherwise = n * factorial (n - 1)

-- 'cantDivisoresPrimos' toma un número entero positivo y devuelve la cantidad de sus divisores primos.
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length [x | x <- [1..n], n `mod` x == 0, esPrimo x]
    where
        esPrimo :: Int -> Bool
        esPrimo m = m > 1 && all (\p -> m `mod` p /= 0) [2..m - 1]


-- Ejercicio 3
{- 
a. Definir la función inverso :: Float → Maybe Float que dado un número devuelve su inverso multiplicativo
si está definido, o Nothing en caso contrario.
-}

inverso :: Float -> Maybe Float

inverso x | x /= 0 = Just ( 1 / x)
          | otherwise = Nothing


{- 
b. Definir la función aEntero :: Either Int Bool → Int que convierte a entero una expresión que puede ser
booleana o entera. En el caso de los booleanos, el entero que corresponde es 0 para False y 1 para True.
-}




aEntero :: Either Int Bool -> Int

aEntero (Right True) = 1
aEntero (Right False) = 0
aEntero (Left n)  = n

-- para probar tengo que aclarar en el ejemplo si es Right o Left

{-
Ejercicio 4
Definir las siguientes funciones sobre listas:
a. limpiar :: String → String → String, que elimina todas las apariciones de cualquier carácter de la primera
cadena en la segunda. Por ejemplo, limpiar ``susto'' ``puerta'' evalúa a ``pera''. Nota: String es un
renombre de [Char]. La notación ``hola'' es equivalente a [`h',`o',`l',`a'] y a `h':`o':`l':`a':[].
b. difPromedio :: [Float] → [Float] que dada una lista de números devuelve la diferencia de cada uno con el
promedio general. Por ejemplo, difPromedio [2, 3, 4] evalúa a [-1, 0, 1].
c. todosIguales :: [Int] → Bool que indica si una lista de enteros tiene todos sus elementos iguales.
-}

limpiar :: String -> String -> String
limpiar letrasaborrar str = filter (\c -> not (c `elem` letrasaborrar)) str

difPromedio :: [Float] -> [Float]

difPromedio l = map (\x -> (x - (promedio l))) l

promedio :: [Float] -> Float
promedio l = (foldl (\acc x -> acc + x) 0 l) / fromIntegral(length l)

todosIguales :: [Int] -> Bool
todosIguales l = all (\x -> all(\e -> x==e) l) l

{-
Ejercicio 5
Dado el siguiente modelo para árboles binarios:
data AB a = Nil | Bin (AB a) a (AB a)
denir las siguientes funciones:
a. vacioAB :: AB a → Bool que indica si un árbol es vacío (i.e. no tiene nodos).
b. negacionAB :: AB Bool → AB Bool que dado un árbol de booleanos construye otro formado por la negación
de cada uno de los nodos.
c. productoAB :: AB Int → Int que calcula el producto de todos los nodos del árbol.

-}
data AB a = Nil | Bin (AB a) a (AB a)
-- Función vacioAB para verificar si un árbol binario está vacío
vacioAB :: AB a -> Bool
vacioAB Nil = True     -- Un árbol Nil (vacío) es verdadero
vacioAB (Bin _ _ _) = False  -- Un árbol Bin (con cualquier subárbol y valor) no está vacío

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil  -- Si el árbol es vacío, devuelve un árbol vacío
negacionAB (Bin izq x der) = Bin (negacionAB izq) (not x) (negacionAB der)

productoAB :: AB Int -> Int

productoAB Nil = 1
productoAB (Bin izq x der) = x * productoAB(izq) * productoAB(der)