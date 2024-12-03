data AB a = Nil | Bin (AB a) a (AB a) deriving(Show)

foldAB ::
    b                       -- Nil
    -> (b -> a -> b -> b)   -- Bin
    -> AB a
    -> b

foldAB z f x = case x of
    Nil -> z
    (Bin l v r) -> f (rec l) v (rec r)
    where rec = foldAB z f


recAB ::
    b                                       -- Nil
    -> (AB a -> a -> AB a -> b -> b -> b)   -- Bin
    -> AB a
    -> b

recAB z f x = case x of
    Nil -> z
    (Bin l v r) -> f l v r (rec l) (rec r)
    where rec = recAB z f
esNil :: AB a -> Bool
esNil x = case x of
    Nil -> True
    _ -> False

mejorSegún :: (a -> a -> Bool) -> AB a -> a
mejorSegún f (Bin l v r) = foldAB v (\rl v rr -> (rl `g` v) `g` rr) (Bin l v r)
    where g x y = if f x y then x else y

mejorSegún :: (a -> a -> Bool) -> AB a -> a
mejorSegún f (Bin l v r) = foldAB v (\rl v rr -> if f (if f rl v then rl else v) rr then (if f rl v then rl else v) else rr) (Bin l v r)

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\ l v r rl rr -> (esNil l || mejorSegún (>) l <= v) && (esNil r || mejorSegún (<) r >= v) && rl && rr)



raíz :: AB a -> a
raíz (Bin l v r) = v

arbolprueba = Bin (Bin (Bin Nil 2 Nil) 8 (Bin Nil 11 Nil)) 10 Nil 