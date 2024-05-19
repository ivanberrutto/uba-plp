{-
Ejercicio 1 ⋆
Considerar las siguientes definiciones de funciones:
- max2 (x, y) | x >= y = x
| otherwise = y
- normaVectorial (x, y) = sqrt (x^2 + y^2)
- subtract = flip (-)
- predecesor = subtract 1
- evaluarEnCero = \f -> f 0
- dosVeces = \f -> f . f
- flipAll = map flip
- flipRaro = flip flip
i. ¿Cuál es el tipo de cada función? (Suponer que todos los números son de tipo Float).
ii. Indicar cuáles de las funciones anteriores no están currificadas. Para cada una de ellas, definir la función
currificada correspondiente. Recordar dar el tipo de la función.
-}
{-
max2 :: (Float,Float) -> Float
normaVectorial :: (Float,Float) -> Float
subtract :: Float -> Float -> Float
predecesor :: Float -> Float
evaluarEnCero :: (Float -> b) -> b
dosVeces :: (a -> b) -> c
flipAll :: ([a] -> [b]-> c) ->  [b] -> [a]-> c
-}

{-
Ejercicio 2 ⋆
i. Definir la función curry, que dada una función de dos argumentos, devuelve su equivalente currificada.
ii. Definir la función uncurry, que dada una función currificada de dos argumentos, devuelve su versión no
currificada equivalente. Es la inversa de la anterior.
iii. ¿Se podría definir una función curryN, que tome una función de un número arbitrario de argumentos y
devuelva su versión currificada?
Sugerencia: pensar cuál sería el tipo de la función.
-}


curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \x -> \y -> f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y


-- No se podria definir un curryN pero si podes hacer un curry de la funcion con n argumentos que quieras currificar
-- 	 Como por ejemplo un curry3


{-
Ejercicio 3 ⋆
i. Redefinir usando foldr las funciones sum, elem, (++), filter y map.
ii. Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún
(>).
iii. Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ❀ [1,5,4,4,9].
iv. Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
v. Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
etc.). Pensar qué esquema de recursión conviene usar en este caso.

-}

misum :: [Float] -> Float
misum s = foldr (\acc x -> acc + x) 0 s

misum2 :: [Int] -> Int
misum2 s = foldr (+) 0 s

mielem :: Eq a => a -> [a] -> Bool
mielem e s = foldr (\x acc -> acc || e==x) False s

mimasmas :: [a] -> [a] -> [a]
mimasmas s1 s2 = foldr(\x acc -> x:acc) s2 s1

mifilter :: (a -> Bool) -> [a] -> [a]
mifilter f s = foldr(\x acc -> if f x  then x:acc else acc) [] s

mimap :: (a -> b) -> [a] -> [b] 
mimap f s = foldr(\x acc -> (f x):acc) [] s

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f s = foldr1 (\x acc -> if (f x acc) then x else acc) s

sumasParciales :: Num a => [a] -> [a]
sumasParciales = scanl1 (+)

sumaAlt :: Num a => [a] -> a
sumaAlt l = fst $ foldr (\x (acc1,acc2) -> if acc2 then (acc1+x,False) else (acc1-x,True) ) ((reverse l !! 0),False) (init l) 

sumaAltPiza :: Num a => [a] -> a
sumaAltPiza = foldr (-) 0

--sumaAlt2::? 


-- ej 4: -

-- ej 5: -

{-
Ejercicio 6 ⋆
El siguiente esquema captura la recursión primitiva sobre listas.
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)
a. Definir la función sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el
resultado de eliminar de la lista la primera aparición del elemento (si está presente).
b. Explicar por qué el esquema de recursión estructural (foldr) no es adecuado para implementar la función
sacarUna del punto anterior.
c. Definr la función insertarOrdenado :: Ord a => a -> [a] -> [a] que inserta un elemento en una lista
ordenada (de manera creciente), de manera que se preserva el ordenamiento.
-}

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z []       = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarPrimera:: Eq a => a -> [a] -> [a]
sacarPrimera e = recr (\x xs rec -> if e == x then xs else x:rec) [] 

-- b: Porque con foldr no sabes cuando parar

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs rec -> if e < x then e:x:xs else if xs == [] then x:rec ++ [e] else x:rec) [] 

{-
Ejercicio 8 ⋆
Definir las siguientes funciones para trabajar sobre listas, y dar su tipo. Todas ellas deben poder aplicarse a
listas finitas e infinitas.
i. mapPares, una versión de map que toma una función currificada de dos argumentos y una lista de pares
de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry.
ii. armarPares, que dadas dos listas arma una lista de pares que contiene, en cada posición, el elemento
correspondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,
ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en
Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial.
iii. mapDoble, una variante de mapPares, que toma una función currificada de dos argumentos y dos listas
(de igual longitud), y devuelve una lista de aplicaciones de la función a cada elemento correspondiente de
las dos listas. Esta función en Haskell se llama zipWith.


-

-}


{-
Ejercicio 10 ⋆
Definimos la función generate, que genera listas en base a un predicado y una función, de la siguiente manera:
generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []
generateFrom:: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs = init xs
| otherwise = generateFrom stop next (xs ++ [next xs])
i. Usando generate, definir generateBase::([a] -> Bool) -> a -> (a -> a) -> [a], similar a
generate, pero con un caso base para el elemento inicial, y una función que, en lugar de calcular el siguiente
elemento en base a la lista completa, lo calcula a partir del último elemento. Por ejemplo: generateBase
(\l->not (null l) && (last l > 256)) 1 (*2) es la lista las potencias de 2 menores o iguales que 256.
ii. Usando generate, definir factoriales::Int -> [Int], que dado un entero n genera la lista de los
primeros n factoriales.
iii. Usando generateBase, definir iterateN :: Int -> (a -> a) -> a -> [a] que, toma un entero n, una
función f y un elemento inicial x, y devuelve la lista [x, f x, f (f x), ..., f ( ...(f x) ...)] de
longitud n. Nota: iterateN n f x = take n (iterate f x).
iv. Redefinir generateFrom usando iterate y takeWhile.
-}

{-
Ejercicio 11
Ejercicio 11 ⋆
i. Denir y dar el tipo del esquema de recursión foldNat sobre los naturales. Utilizar el tipo Integer de
Haskell (la función va a estar denida sólo para los enteros mayores o iguales que 0).
ii. Utilizando foldNat, denir la función potencia.
-} 
data Nat  = Zero | Succ Nat 


foldNat :: (a -> a) -> a -> Integer -> a
foldNat f z 0 = z
foldNat f z n = f (foldNat f z (n - 1))

potencia :: Integer -> Integer -> Integer
potencia x n = foldNat (\acc -> x * acc) 1 n

{-
Ejercicio 13 ⋆
Considerar el siguiente tipo, que representa a los árboles binarios:
data AB a = Nil | Bin (AB a) a (AB a)
i. Usando recursión explícita, denir los esquemas de recursión estructural (foldAB) y primitiva (recAB), y
dar sus tipos.
ii. Denir las funciones esNil, altura y cantNodos (para esNil puede utilizarse case en lugar de foldAB
o recAB).
-}

data AB a = Nil | Bin (AB a) a (AB a)
{-
foldAB ::  b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cAB arbol = case arbol of
	Nil -> cNil
	Bin izq r der -> cAB (rec izq) r (rec der)
	where rec = foldAB cNil cAB 
-}
recAB :: (a -> AB -> AB -> b) -> b -> AB -> b 
recAB f1 f2 arbol = case arbol of
	Bin r izq der -> f1 r izq der (rec izq) (rec der)
	Nil -> f2
  where rec = recAB f1 f2

foldAB :: (a -> b -> b -> b) -> b -> AB a -> b 
foldAB f1 f2 = recAB (\a _ _ -> f1 a) f2