type Procesador a b = a -> [b]

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose x hijos) = f x (map (foldRose f) hijos)

-- data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie f (TrieNodo x y)= f x (map (\(c, t) -> (c, foldTrie f t)) y)
--

unoxuno :: Procesador [a] [a]
unoxuno  = foldr (\x rec -> [x]:rec) [] 

sufijos :: Procesador [a] [a]
sufijos = foldr (\x acc -> (x : head acc) : acc) [[]]

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\x hijos -> x : concat hijos)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\x hijos -> if null hijos then [x] else concat hijos)

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\x hijos -> if null hijos then [[x]] else map (x:) (concat hijos))

--foldTrie f (TrieNodo x y)= f x (map (foldTrie f) y )
{-
caminos :: Trie a -> [b]
caminos =   foldTrie(\x hijos -> case x of
  Nothing -> concat (map snd hijos)
  Just y ->  y : concat (map snd hijos) 
  )
-}
--Ejercicio 6
{-
caminos :: Trie a -> [Char]
caminos = foldTrie (\_ hijos -> if null hijos then [' '] else concat (map (\(c, ch) -> c : ch) hijos) ) 
-}
caminos :: Trie a -> [String]
caminos = foldTrie (\_ hijos ->  concat(map (\(c, ch) -> c : ch)  hijos ) : concat(map (snd)hijos)) 
{-
caminos :: Trie a -> [String]
caminos = foldTrie (\_ hijos ->  (map (fst) (concat hijos) )  ++  concat (map snd hijos) )
-}
{-
caminos :: Trie a -> [String]
caminos =   foldTrie(\x hijos -> case x of
  Nothing -> concatMap snd hijos
  Just y -> (fst hijos) : concatMap snd hijos
  )
-}

t = TrieNodo Nothing[('a', TrieNodo (Just True) []),('b', TrieNodo Nothing[('a', TrieNodo (Just True)[('d', TrieNodo Nothing [])])]),('c', TrieNodo (Just True) [])]
