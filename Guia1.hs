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
max2 :: (Float,Float) -> Float
normaVectorial :: (Float,Float) -> Float
subtract :: Float -> Float -> Float
predecesor :: Float -> Float
evaluarEnCero :: (Float -> b) -> b
dosVeces :: (a -> b) -> c
flipAll :: ([a] -> [b]-> c) ->  [b] -> [a]-> c

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