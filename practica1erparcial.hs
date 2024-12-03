

data AB a = Nil | Bin (AB a) a (AB a)
{-
foldAB ::  b -> (b -> a -> b -> b) -> AB a -> b
foldAB cAB cNil arbol = case arbol of
	Nil -> cNil
	Bin izq r der -> cAB (rec izq) r (rec der)
	where rec = foldAB cAB cNil
-}

data Matriz a = NuevaMatriz a |  Agregar a Int Int (Matriz a)

foldMatriz :: ( a -> b ) -> ( a -> Int -> Int -> b -> b ) -> Matriz a -> b

foldMatriz cNuevaMatriz cAgregar m = case m of
	NuevaMatriz x -> cNuevaMatriz x 
	Agregar a f c m2 -> cAgregar a f c (rec m2)
		where rec = foldMatriz cNuevaMatriz cAgregar 

ver :: Int -> Int -> Matriz a -> a
ver f c m = foldMatriz id (\a fm cm mrec -> if (f == fm && c == cm) then a else mrec) m

m2 = Agregar 'a' 1 2 $ Agregar 'b' 1 2 $ Agregar 'c' 1 1 $ NuevaMatriz 'd'



data HashSet a = Hash (a -> Integer) -> (Integer -> [a])

vacio :: (a -> Integer ) -> Hashset a
vacio f = Hash f (\x -> [])

pertenece:: Eq a => a -> HashSet a -> Bool

pertenece x (Hash h f ) = elem x (f (h a))

agregar :: Eq => a -> HashSet a -> HashSet a

agregar x (Hash h f) = if (pertenece x (Hash h f)) then (Hash h f) else (Hash h (\y -> if (h x )= y then x:f(h x) else f y))

data Componente = Contenedor | Motor | Escudo | Cañon deriving Eq

data NaveEspacial = Modulo Componente NaveEspacial NaveEspacial | Base Componente deriving Eq

recNave :: ( Componente -> NaveEspacial -> NaveEspacial -> a -> a -> a) -> ( Componente -> a ) -> NaveEspacial -> a


recNave f1 f2 n = case n of
	Modulo c n1 n2 -> f1 c n1 n2 ( rec n1 ) ( rec n2 )
	Base c -> f2 c
	where rec = recNave f1 f2

foldNave :: ( Componente -> a -> a -> a )
-> ( Componente -> a ) -> NaveEspacial -> a

foldNave f1 f2 = recNave (\ c _ _ -> f1 c ) f2

espejo :: NaveEspacial -> NaveEspacial
espejo = foldNave (\ c r1 r2 -> M ´o dulo c r2 r1 ) Base

esSubnavePropia :: NaveEspacial -> NaveEspacial -> Bool
esSubnavePropia n1 = recNave (\ _ sn1 sn2 r1 r2 -> sn1 == n1 ||sn2 == n1 || r1 || r2 )( const False )

truncar :: NaveEspacial -> Integer -> NaveEspacial
truncar =foldNave (\ c r1 r2 -> \ i -> if i == 0 then ( Base c ) else ( Modulo c ( r1 (i -1) ) ( r2 (i -1) ) ) ) (\ c -> \ i -> Base c )

