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