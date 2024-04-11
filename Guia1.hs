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