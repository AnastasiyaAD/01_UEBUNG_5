> module Mengen where
> type Element = Char -- Datentyp für Mengenglieder
> newtype MT1 = MT1 [Element] -- Datentyp für Mengen, repräsentiert als Listen
> data MT2 = Nichts
>            | VerlaengereUm Element MT2
> newtype MT3 = MT3 (Element -> Bool) 

> class Menge a where -- Die Typklasse `Menge` definiert eine gemeinsame Schnittstelle für Mengen verschiedener Typen.
>   leereMenge :: a
>   allMenge :: a -- Universelle Menge
>   istMenge :: a -> Bool -- Prüfung: Ist der gegebene Wert eine Menge?
>   vereinige :: a -> a -> a -- Vereinigung zweier Mengen
>   schneide :: a -> a -> a -- Schnittmenge zweier Mengen
>   zieheab :: a -> a -> a -- Differenz zweier Mengen
>   komplementiere :: a -> a -- {'a','d','c'}: {'d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'}
>   sindGleich :: a -> a -> Bool
>   istTeilmenge :: a -> a -> Bool
>   istObermenge :: a -> a -> Bool
>   zeige :: a -> String
> -------------------------------------------------------------------- (a) --------------------------------------------------------------
> instance Menge MT1 where
>   leereMenge = MT1 []
>   allMenge = MT1 (['a'..'z'] ++ ['A'..'Z'])
>   istMenge (MT1 []) = True
>   istMenge (MT1 xs)= 
>       if length xs == length (removeDuplicates xs) then istMengeMT1 xs
>       else False
>   vereinige (MT1 xs) (MT1 ys) = 
>       if istMenge (MT1 xs) && istMenge(MT1 ys) then MT1 (removeDuplicates (xs ++ ys))
>       else error "Ungültige Elemente"
>   schneide (MT1 xs) (MT1 ys) = 
>       if istMenge (MT1 xs) && istMenge(MT1 ys) then MT1 (duplicates (xs ++ ys))
>       else error "Ungültige Elemente"
>   zieheab (MT1 xs) (MT1 ys) = 
>       if istMenge (MT1 xs) && istMenge(MT1 ys) then MT1 (filter (`notElem` (duplicates (xs ++ ys))) (removeDuplicates (xs ++ ys))) -- zieheab {'a','d','c'} {'a','c'} : {'b'}
>       else error "Ungültige Elemente"
>   komplementiere (MT1 xs) = 
>       if istMenge (MT1 xs) then zieheab (MT1 xs) (allMenge :: MT1)
>       else error "Ungültige Elemente"
>   sindGleich (MT1 xs) (MT1 ys) =
>       if istMenge (MT1 xs) && istMenge(MT1 ys) then xs == ys
>       else error "Ungültige Elemente"
>   istTeilmenge (MT1 xs) (MT1 ys) =
>       if istMenge (MT1 xs) && istMenge(MT1 ys) then xs == duplicates (xs ++ ys) || sindGleich (MT1 xs) (leereMenge :: MT1)
>       else error "Ungültige Elemente"
>   istObermenge (MT1 xs) (MT1 ys) =
>       if istMenge (MT1 xs) && istMenge(MT1 ys) then ys == duplicates (xs ++ ys) || sindGleich (MT1 xs) (allMenge :: MT1)
>       else error "Ungültige Elemente"
>   zeige :: MT1 -> String
>   zeige (MT1 []) = "{}"  -- Fall für eine leere Liste
>   zeige (MT1 [x]) = "{" ++ show x ++ "}" -- Fall für eine Liste mit einem Element
>   zeige (MT1 (x:xs)) = "{" ++ show x ++ (concatMap (\y -> "," ++  show y ) xs) ++ "}"

> removeDuplicates :: Eq a => [a] -> [a] -- Entfernt Duplikate aus einer Liste.
> removeDuplicates [] = []
> removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs) -- Rekursiver Fall:  x wird hinzugefügt, dann werden Duplikate von x aus dem Rest der Liste entfernt.

> duplicates :: (Ord a) => [a] -> [a] -- Findet Elemente, die mehr als einmal in einer Liste vorkommen.
> duplicates xs = map fst $ filter (\(x, count) -> count > 1) counts
>   where
>       counts = countOccurrences xs  -- Hilfsfunktion, um die Vorkommnisse jedes Elements zu zählen.
>       countOccurrences [] = []
>       countOccurrences (x:xs) = (x, 1 + length (filter (== x) xs)) : countOccurrences (filter (/= x) xs) -- Rekursiver Fall:  Zählt Vorkommnisse von x und ruft sich selbst rekursiv für den Rest der Liste auf, nachdem x entfernt wurde.

> istMengeMT1 :: [Element] -> Bool
> istMengeMT1 [] = True
> istMengeMT1 (x:xs) =
>   if x `elem` (['a'..'z'] ++ ['A'..'Z']) then istMengeMT1 xs
>   else False
> -------------------------------------------------------------------- (b) --------------------------------------------------------------
> instance Menge MT2 where
>   leereMenge = Nichts
>   allMenge = createMT2 (['a'..'z'] ++ ['A'..'Z'])
>   istMenge Nichts = True
>   istMenge x = istMenge (MT1(toListeMT2 x))
>   vereinige x y = 
>       if istMenge x && istMenge y then createMT2 (removeDuplicates (summMT2 x y))
>       else error "Ungültige Elemente"
>   schneide x y =
>       if istMenge x && istMenge y then createMT2 (duplicates (summMT2 x y))
>       else error "Ungültige Elemente"
>   zieheab x y =
>      if istMenge x && istMenge y then createMT2 (filter (`notElem` (duplicates (summMT2 x y))) (removeDuplicates (summMT2 x y))) -- zieheab {'a','d','c'} {'a','c'} : {'b'}
>       else error "Ungültige Elemente"
>   komplementiere x = 
>       if istMenge x then zieheab x (allMenge :: MT2)
>       else error "Ungültige Elemente"
>   sindGleich x y =
>       if istMenge x && istMenge y then toListeMT2 x == toListeMT2 y
>       else error "Ungültige Elemente"
>   istTeilmenge x y =
>       if istMenge x && istMenge y then toListeMT2 x == duplicates (summMT2 x y) || sindGleich x (leereMenge :: MT2)
>       else error "Ungültige Elemente"
>   istObermenge x y =
>       if istMenge x && istMenge y then toListeMT2 y == duplicates (summMT2 x y) || sindGleich x (allMenge :: MT2)
>       else error "Ungültige Elemente"
>   zeige :: MT2 -> String
>   zeige (Nichts) = "{}"
>   zeige (VerlaengereUm x xs) = zeige (MT1(toListeMT2(VerlaengereUm x xs)))

> createMT2 :: [Char] -> MT2
> createMT2 [x] = VerlaengereUm x Nichts
> createMT2 (x:xs) = VerlaengereUm x (createMT2(xs))

> toListeMT2' :: MT2 -> [Char]
> toListeMT2' (VerlaengereUm z n) = toListeMT2' n ++ [z]
> toListeMT2' (Nichts) = []

> toListeMT2 :: MT2 -> [Char]
> toListeMT2 x = reverse (toListeMT2' x)

> summMT2 :: MT2 -> MT2 -> [Char]
> summMT2 x y = toListeMT2 x ++ toListeMT2 y


> main :: IO ()
> main = do
>   putStrLn $ "----------------------------(a)---------------------------"
>   let m1 = MT1['a','b','c']
>   let m1'= MT1['b','e']
>   let m1''= MT1['a','c']
>   putStrLn $ "leereMenge MT1 : " ++ zeige (leereMenge :: MT1)
>   putStrLn $ ""
>   putStrLn $ "allMenge MT1 : " ++ zeige (allMenge :: MT1)
>   putStrLn $ ""
>   putStrLn $ "vereinige {'a','b','c'} {'b','e'} : " ++ zeige (vereinige m1 m1')
>   putStrLn $ ""
>   putStrLn $ "schneide {'a','d','c'} {'a','c'} : " ++ zeige (schneide m1 m1'')
>   putStrLn $ ""
>   putStrLn $ "zieheab {'a','d','c'} {'a','c'} : " ++ zeige (zieheab m1 m1'')
>   putStrLn $ ""
>   putStrLn $ "komplementiere {'a','d','c'} : " ++ zeige (komplementiere m1)
>   putStrLn $ ""
>   putStrLn $ "sindGleich {'a','d','c'} {'a','d','c'} : " ++ show (sindGleich m1 m1)
>   putStrLn $ "sindGleich {'a','d','c'} {'a','c'} : " ++ show(sindGleich m1 m1'')
>   putStrLn $ ""
>   putStrLn $ "istTeilmenge {'a','d','c'} {'a','d','c'} : " ++ show (istTeilmenge m1 m1)
>   putStrLn $ "istTeilmenge {'a','c'} {'a','d','c'}: " ++ show(istTeilmenge m1'' m1)
>   putStrLn $ "istTeilmenge {'a','c'} {'b','e'}: " ++ show(istTeilmenge m1'' m1')
>   putStrLn $ "istTeilmenge {} {'a','c'} : " ++ show(istTeilmenge (leereMenge :: MT1) m1'' )
>   putStrLn $ ""
>   putStrLn $ "istObermenge {'a','d','c'} {'a','d','c'} : " ++ show (istObermenge m1 m1)
>   putStrLn $ "istObermenge {'a','d','c'} {'a','c'}: " ++ show(istObermenge m1 m1'')
>   putStrLn $ "istObermenge {'a','c'} {'b','e'}: " ++ show(istObermenge m1'' m1')
>   putStrLn $ "istObermenge allMenge MT1 {'a','c'} : " ++ show(istObermenge (allMenge :: MT1) m1'' )
>   putStrLn $ ""
>   putStrLn $ "zeige (MT1 'aabc') : " ++ zeige (MT1 "aabc")
>   putStrLn $ "----------------------------(b)---------------------------"
>   let m2 = createMT2 ['a','b','c']
>   let m2'= createMT2 ['b','e']
>   let m2''= createMT2 ['a','c']
>   putStrLn $ "leereMenge MT2 : " ++ zeige (leereMenge :: MT2)
>   putStrLn $ ""
>   putStrLn $ "allMenge MT2 : " ++ zeige (allMenge :: MT2)
>   putStrLn $ ""
>   putStrLn $ "vereinige {'a','b','c'} {'b','e'} : " ++ zeige (vereinige m2 m2')
>   putStrLn $ ""
>   putStrLn $ "schneide {'a','d','c'} {'a','c'} : " ++ zeige (schneide m2 m2'')
>   putStrLn $ ""
>   putStrLn $ "zieheab {'a','d','c'} {'a','c'} : " ++ zeige (zieheab m2 m2'')
>   putStrLn $ ""
>   putStrLn $ "komplementiere {'a','d','c'} : " ++ zeige (komplementiere m2)
>   putStrLn $ ""
>   putStrLn $ "sindGleich {'a','d','c'} {'a','d','c'} : " ++ show (sindGleich m2 m2)
>   putStrLn $ "sindGleich {'a','d','c'} {'a','c'} : " ++ show(sindGleich m2 m2'')
>   putStrLn $ ""
>   putStrLn $ "istTeilmenge {'a','d','c'} {'a','d','c'} : " ++ show (istTeilmenge m2 m2)
>   putStrLn $ "istTeilmenge {'a','c'} {'a','d','c'}: " ++ show(istTeilmenge m2'' m2)
>   putStrLn $ "istTeilmenge {'a','c'} {'b','e'}: " ++ show(istTeilmenge m2'' m2')
>   putStrLn $ "istTeilmenge {} {'a','c'} : " ++ show(istTeilmenge (leereMenge :: MT2) m2'' )
>   putStrLn $ ""
>   putStrLn $ "istObermenge {'a','d','c'} {'a','d','c'} : " ++ show (istObermenge m2 m2)
>   putStrLn $ "istObermenge {'a','d','c'} {'a','c'}: " ++ show(istObermenge m2 m2'')
>   putStrLn $ "istObermenge {'a','c'} {'b','e'}: " ++ show(istObermenge m2'' m2')
>   putStrLn $ "istObermenge allMenge MT2 {'a','c'} : " ++ show(istObermenge (allMenge :: MT2) m2'' )
>   putStrLn $ ""
>   putStrLn $ "zeige (VerlaengereUm 'a' (VerlaengereUm 'c' (Nichts))) : " ++ zeige (VerlaengereUm 'a' (VerlaengereUm 'c' (Nichts)))
>   putStrLn $ "----------------------------(b)---------------------------"
