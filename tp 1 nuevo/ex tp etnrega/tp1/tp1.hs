import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)

type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)

data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)

type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)


{-Ejercicio 1-}

foldPersonaje :: (Posición -> String -> p) -> (p -> Dirección -> p) -> (p -> p) -> Personaje -> p
foldPersonaje casoPersonaje casoMueve casoMuere persona = case persona of
  Personaje pos nom -> casoPersonaje pos nom
  Mueve per dir -> casoMueve (foldPersonaje casoPersonaje casoMueve casoMuere per) dir
  Muere per -> casoMuere (foldPersonaje casoPersonaje casoMueve casoMuere per)

foldObjeto :: (Posición -> String -> b) -> (b -> Personaje -> b) -> (b -> b) -> Objeto -> b
foldObjeto casoObjeto casoTomado casoDestruido objeto = case objeto of
  Objeto pos nom -> casoObjeto pos nom
  Tomado obj per -> casoTomado (foldObjeto casoObjeto casoTomado casoDestruido obj) per
  EsDestruido obj -> casoDestruido (foldObjeto casoObjeto casoTomado casoDestruido obj)

{-Ejercicio 2-}

posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje const siguiente_posición id

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (const id) const id

{-Ejercicio 3-}

objetos_en :: Universo -> [Objeto]
objetos_en [] = []
objetos_en u = map objeto_de (filter (es_un_objeto) u)

personajes_en :: Universo -> [Personaje]
personajes_en u = map personaje_de (filter (es_un_personaje) u)

{-Ejercicio 4-}

objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de n u = filter (en_posesión_de n) (objetos_en u)

{-Ejercicio 5-}

-- Asume que hay al menos un objeto LIBRE
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano p u = foldr1 (\x acc -> if (<) (distancia (Left p) (Right x)) (distancia (Left p) (Right acc)) then x else acc) (objetos_libres_en u)

{-Ejercicio 6-}

tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = length(filter (es_una_gema) (filter (en_posesión_de "Thanos" ) (objetos_en u))) == 6

{-Ejercicio 7-}

podemos_ganarle_a_thanos :: Universo -> Bool
podemos_ganarle_a_thanos u = thanos_no_consiguio_todas_las_gemas u && (thor_puede_vencerlo u || wanda_puede_destruir_la_gema u)

thanos_no_consiguio_todas_las_gemas :: Universo -> Bool
thanos_no_consiguio_todas_las_gemas u = not(tiene_thanos_todas_las_gemas u)

thor_puede_vencerlo :: Universo -> Bool
thor_puede_vencerlo u = (está_el_personaje "Thor" u) && (está_el_objeto "StormBreaker" u)

wanda_puede_destruir_la_gema :: Universo -> Bool
wanda_puede_destruir_la_gema u = (está_el_personaje "Wanda" u) && (está_el_personaje "Visión" u) && (en_posesión_de "Visión" (objeto_de_nombre "Gema de la Mente" u))

--Asumimos que siempre va a existir exactamente una Gema de la Mente en el universo. Si no podriamos agregar un
  --if (existe_la_gema u) then (wanda_puede_destruir_la_gema u) else False.
--Ya que si no en wanda_puede_destruir_la_gema invoca a la función objeto_de_nombre que no funciona si el objeto no pertenece al universo
existe_la_gema :: Universo -> Bool
existe_la_gema u = if ( length(filter ("Gema de la Mente"==) (map (nombre_objeto) (objetos_en u))) > 0) then True else False


{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

--Personajes=
thanos = Personaje (0,1) "Thanos"
thor = Personaje (0,2) "Thor" 
wanda = Personaje (0,3) "Wanda"
vision = Personaje (0,4) "Visión"
tini = Personaje (0,5) "Tini"
mueveDuki = Mueve (Personaje (0,6) "Duki") Norte
muerePhil = Muere (Personaje (0,7) "Phil")

--Objetos=
obj1 = Objeto (2,0) "StormBreaker"
obj2 = Objeto (2,1) "Libro"
obj3 = Objeto (2,2) "Computadora"
gema = Objeto (2,3) "Gema de la Mente"

objDestruido = EsDestruido (Objeto (0,2) "Cuaderno") 

objDeWanda = Tomado (Objeto (0,3) "Campera Jean") (wanda)
objDeThor = Tomado (Objeto (0,2) "Cuaderno") (thor)
objDeThanos1 = Tomado (Objeto (0,1) "Te") (thanos)
objDeThanos2 = Tomado (Objeto (0,1) "Cafe") (thanos)
objDeThanos3= Tomado (Objeto (0,1) "Buzo Oversize") (thanos)
objDeThanos4 = Tomado (Objeto (0,1) "Pantalon Cargo") (thanos)

gemaDeThanos1 = Tomado (Objeto (0,1) "Gema de la Mente") (thanos)
gemaDeThanos2 = Tomado (Objeto (0,1) "Gema de alma") (thanos)
gemaDeThanos3 = Tomado (Objeto (0,1) "Gema de espacio") (thanos)
gemaDeThanos4 = Tomado (Objeto (0,1) "Gema de poder") (thanos)
gemaDeThanos5 = Tomado (Objeto (0,1) "Gema de tiempo") (thanos)
gemaDeThanos6 = Tomado (Objeto (0,1) "Gema de realidad") (thanos)

gemaDeVision1 = Tomado (Objeto (0,4) "Gema de la Mente") (vision)
gemaDeVision2 = Tomado (Objeto (0,4) "Gema de la Mente") (vision)
gemaDeVision3 = Tomado (Objeto (0,4) "Gema de la Mente") (vision)
gemaDeVision4 = Tomado (Objeto (0,4) "Gema de la Mente") (vision)
gemaDeVision5 = Tomado (Objeto (0,4) "Gema de la Mente") (vision)
gemaDeVision6 = Tomado (Objeto (0,4) "Gema de la Mente") (vision)

--Universos= 
universo_vacio = []
universo_sin_objetos = universo_con [thanos] []
universo_sin_personajes = universo_con [] [obj2,obj3] 
universo_todos_obj_libres = universo_con [thanos] [obj1,obj2,obj3]
universo = universo_con [thor, wanda, vision, mueveDuki, muerePhil] [obj1,objDestruido,objDeThanos1,objDeWanda,objDeThor] --Universo con todo tipo de objetos y personajes

--Tests=

testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) thanos       --Personaje de caso Personaje Posición String
    ~=? 0
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) mueveDuki    --Personaje de caso Mueve Personaje Dirección
    ~=? 1
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) muerePhil    --Personaje de caso Muere Personaje
    ~=? 1
  ,
  foldObjeto (\p s -> 0) (\r d -> r+1) (\r -> r+1) obj1            --Objeto de tipo Objeto Posición String
    ~=? 0
  ,
  foldObjeto (\p s -> 0) (\r d -> r+1) (\r -> r+1) objDeThor       --Objeto de tipo Tomado Objeto Personaje
    ~=? 1
  ,
  foldObjeto (\p s -> 0) (\r d -> r+1) (\r -> r+1) objDestruido    --Objeto de tipo EsDestruido Objeto
    ~=? 1
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje wanda        --Personaje de caso Personaje Posición String
    ~=? (0,3)
  ,
  posición_personaje mueveDuki    --Personaje de caso Mueve Personaje Dirección
    ~=? (0,7)
  ,
  posición_personaje muerePhil    --Personaje de caso Muere Personaje
    ~=? (0,7)
  ,
  nombre_objeto obj2              --Objeto de caso Objeto Posición String
    ~=? "Libro"
  ,
  nombre_objeto objDestruido      --Objeto de caso esDestruido Objeto
    ~=? "Cuaderno"
  ,
  nombre_objeto objDeThanos3      --Objeto de caso Tomado Objeto Personaje
    ~=? "Buzo Oversize"
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en universo_vacio
    ~=? []
  ,
  objetos_en universo_sin_objetos
    ~=? []
  ,
  objetos_en universo       -- Universo con todo tipo de objetos
    ~=? [Objeto (2,0) "StormBreaker",EsDestruido (Objeto (0,2) "Cuaderno"),Tomado (Objeto (0,1) "Te") (thanos), Tomado (Objeto (0,3) "Campera Jean") (wanda), Tomado (Objeto (0,2) "Cuaderno") (thor)]
  ,
  personajes_en universo_vacio
    ~=? []
  ,
  personajes_en universo_sin_personajes
    ~=? []
  ,
  personajes_en universo    -- Universo con todo tipo de personajes
    ~=? [Personaje (0,2) "Thor", Personaje (0,3) "Wanda", Personaje (0,4) "Visión", Mueve (Personaje (0,6) "Duki") Norte, Muere (Personaje (0,7) "Phil")]
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de "Tini" universo        --nombre no pertenece al universo y no tiene objetos 
    ~=? []
  ,
  objetos_en_posesión_de "Visión" universo      --nombre pertenece al universo y no tiene objetos 
    ~=? []
  ,
  objetos_en_posesión_de "Thanos" universo      --nombre no pertenece al universo y tiene objetos tomados
    ~=? [Tomado (Objeto (0,1) "Te") (Personaje (0,1) "Thanos")]
  ,
  objetos_en_posesión_de "Wanda" universo       --nombre pertenece al universo y tiene objetos tomados 
    ~=? [Tomado (Objeto (0,3) "Campera Jean") (Personaje (0,3) "Wanda")]
  ,
  objetos_en_posesión_de "Thor" universo        --nombre pertenece al universo, tiene objeto tomado con el mismo nombre que uno destruido (no importa porque son objetos =/)
    ~=? [Tomado (Objeto (0,2) "Cuaderno") (Personaje (0,2) "Thor")]
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano vision universo                    -- Vision esta en (0,4) objeto mas cercano es uno Tomado(objDeWanda en (0,3)), luego uno destruido (objDestruido en (0,2)) y por ultimo uno libre. Devuelve el libre
    ~=? Objeto (2,0) "StormBreaker"
  ,
  objeto_libre_mas_cercano thanos universo_todos_obj_libres   -- Hay varios objetos libres, devuelve el más cercano
    ~=? Objeto (2,1) "Libro"
  ]

--Universos para el ejercicio 6
u1 = universo_con [thanos] [objDeThanos1,objDeThanos2,objDeThanos3,objDeThanos4,gemaDeThanos5,gemaDeThanos6] 
u2 = universo_con [thanos] [gemaDeVision1,gemaDeVision2,gemaDeVision3,gemaDeVision4,gemaDeVision5,gemaDeVision6]
u3 = universo_con [thanos] [gemaDeThanos1,gemaDeThanos2,gemaDeThanos3,gemaDeThanos4,gemaDeThanos5,gemaDeThanos6]
u4 = universo_con [] [gemaDeThanos1,gemaDeThanos2,gemaDeThanos3,gemaDeThanos4,gemaDeThanos5,gemaDeThanos6]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas universo_vacio
    ~=? False
  ,
  tiene_thanos_todas_las_gemas universo   --Universo sin Thanos
    ~=? False
  ,
  tiene_thanos_todas_las_gemas u1         --Universo con Thanos, él tiene 6 objetos tomados pero no todos son gemas
    ~=? False
  ,
  tiene_thanos_todas_las_gemas u2         --Universo con Thanos, hay 6 gemas tomadas pero no las tiene Thanos 
    ~=? False
  ,
  tiene_thanos_todas_las_gemas u3         --Universo con Thanos con 6 gemas tomadas por él
    ~=? True
  ,
  tiene_thanos_todas_las_gemas u4         --Universo sin Thanos, pero él tiene 6 gemas
    ~=? True
  ]


--Universos para el ejercicio 7
u5 = universo_con [thor,wanda,vision] [obj1,gemaDeVision1]
u6 = universo_con [thor] [obj1,gema]
u7 = universo_con [wanda,vision] [gemaDeVision1]
u8 = universo_con [thor] [gema]
u9 = universo_con [] [obj1,gema]
u10 = universo_con [wanda,vision] [gema] 
u11 = universo_con [wanda] [gemaDeVision1]
u12 = universo_con [vision] [gemaDeVision1]
u13 = universo_con [thanos,wanda,vision] [gemaDeThanos1,gemaDeThanos2,gemaDeThanos3,gemaDeThanos4,gemaDeThanos5,gemaDeThanos6]
u14 = universo_con [thanos,thor,wanda,vision] [obj1,gemaDeThanos1,gemaDeThanos2,gemaDeThanos3,gemaDeThanos4,gemaDeThanos5,gemaDeThanos6]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos u5     --Thanos no tiene todas las gemas. Thor puede vencerlo y wanda puede destruir las gemas
    ~=? True
  ,
  podemos_ganarle_a_thanos u6     --Thanos no tiene todas las gemas. Thor puede vencerlo pero wanda no puede destruir las gemas
    ~=? True
  ,
  podemos_ganarle_a_thanos u7     --Thanos no tiene todas las gemas. Thor no puede vencerlo pero Wanda puede destruir la gema
    ~=? True
  ,
  podemos_ganarle_a_thanos u8     --Thanos no tiene todas las gemas. Esta Thor en el universo pero no el StormBreaker
    ~=? False
  ,
  podemos_ganarle_a_thanos u9     --Thanos no tiene todas las gemas. No esta Thor en el universo pero esta el StormBreaker
    ~=? False
  ,
  podemos_ganarle_a_thanos u10    --Thanos no tiene todas las gemas. Esta Wanda y Visión pero Visión no tiene la Gema de la Mente 
    ~=? False
  ,
  podemos_ganarle_a_thanos u11    --Thanos no tiene todas las gemas. Esta Wanda, hay una Gema de la Mente tomada por Visión pero no esta Visión en el universo.
    ~=? False
  ,
  podemos_ganarle_a_thanos u12    --Thanos no tiene todas las gemas. No esta Wanda, esta Visión y tiene la gema de la mente
    ~=? False
  ,
  podemos_ganarle_a_thanos u13    --Thanos tiene todas las gemas. Thor no puede vencerlo y Wanda no puede destruir la gema
    ~=? False
  ,
  podemos_ganarle_a_thanos u14    --Thanos tiene todas las gemas. Thor puede vencerlo y Wanda no puede destruir la gema (Asumo que hay exactamente una Gema de la Mente, si Thanos tiene todas las gemas entonces Vision no podra tener la de la Mente)
    ~=? False
  ]
