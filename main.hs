-- TRABAJO PRACTICO N°8
-- PUNTO #1: Explique que hace "misterio" y de un nombre mas adecuado
misterio x y
    | (y==0 || x==0) = 0
    | x<0 && y<0 = misterio (-x) (-y)
    | y<0 = misterio y x
    | otherwise = x + misterio x (y-1)

multiplicacion x y
    | (y==0 || x==0) = 0
    | x<0 && y<0 = multiplicacion (-x) (-y)
    | y<0 = multiplicacion y x
    | otherwise = x + multiplicacion x (y-1)

-- PUNTO #2: Implemente en haskell las siguientes funciones
-- #a: que reciba dos números naturales realice el producto entre dichos números mediante sumas sucesivas
prodss x y
    | (x == 0 || y == 0) = 0
    | x > 0 = x + prodss x (y-1)
    | y > 0 = x + prodss (x-1) y

-- #b: que reciba un número positivo y devuelva la suma de sus dígitos
sumadig x
    | (x <= 0) = 0
    | (x >= 1 && x < 10) = x
    | otherwise = (x `mod` 10) + sumadig(x/10)

-- #c: que dada una lista de números enteros y un número X, cuente cuantos elementos de la lista son iguales que X
contariguales x ys = length[i | i <- ys, x == i]

contarigualesalt [] y = 0
contarigualesalt (x:xs) y = if y==x then 1 + contarigualesalt xs y else contarigualesalt xs y

contarigualesalt2 [] y = 0
contarigualesalt2 (x:xs) y = (if y==x then 1  else 0) + contarigualesalt xs y

-- #d: que reciba una lista y un número X y elimine de la lista todos los elementos iguales a X
eliminariguales x ys= [i | i <- ys, x /= i]

-- #e:  que reciba un valor n y devuelva la lista de los primeros n números naturales
listarNaturalesGuards x
    | (x <= 1) = [1]
    | (x > 1) = [x] ++ listarNaturalesGuards(x-1)

listarNaturalesPM 0 = []
listarNaturalesPM 1 = [1] 
listarNaturalesPM n = [n] ++ listarNaturalesPM(n-1)

listarNaturalesLC n = [x | x <- [1..n]]

-- #f : que reciba una lista y un número natural n y retorne una lista sin los primeros n elementos de la lista dada
sublista :: [Int] -> Int -> [Int]
sublista ys x = [i | i <- ys, x < i]

-- #g : que reciba dos listas y devuelva una nueva lista con los elementos de la primera lista que no están en la segunda lista
diferencia :: [Int] -> [Int]-> [Int] -> [Int]
diferencia [] _ [] = []
diferencia (x:xs) (y:ys) z
    | x == y = diferencia xs ys z
    | otherwise = z ++ diferencia xs ys z

existe :: [Int] -> Int -> Bool
existe [] _ = False
existe (x:xs) y
    | x == y = True
    | otherwise = existe xs y

existeG :: [Int] -> Int -> Bool
existeG xs y
    | null xs = False
    | (length xs == 1) = False || ((head xs) == y)
    | (head xs) == y = True
    | otherwise = existeG (tail xs) y

{-- #h: 
    que reciba como parámetros una función f (de un argumento) y una lista y devuelva como 
    resultado la lista recibida en la que cada uno de sus elementos haya sido transformado con la función f
        transformar doble [1,2,3]
        transformar (\x->x*3) [1,2,3]
 --}
transformar :: (a -> b) -> [a] -> [b] 
transformar _ [] = [] 
transformar f (x:xs) = f x : transformar f xs

doble x = 2*x

{-- #i : que, dado un número natural n, construya una lista que contenga los números pares que 
se encuentran en el intervalo [ 0,n]. Use List Comprehension. --}
tablaDePares n = [2*x | x <- [0..(n-1)/2]]

{-- #j : que recibe un predicado p (un predicado es una función que devuelve un valor booleano) y una 
lista de elementos xs y devuelve True si todos los elementos de la lista satisfacen el predicado, caso
contrario retorna False. Realice una versión con Guards, ejecucion (\x x->x>3) [4,5,6] --}

verificarGuards f as
    | null as = False
    | (length as == 1) = if f (head as) then True else False
    | f (head as) = verificarGuards f (tail as)
    | otherwise = False

verificarM [] f = False
verificarM (a:as) f
    | (length as == 0) = if f a then True else False
    | f a = verificarM as f
    | otherwise = False

verificarPM [] f = False
verificarPM (x:xs) f = 
    if (length xs == 0) then f x 
    else if f x then verificarPM xs f else False

{-- #k : que reciba una función f y dos listas y retorne una nueva lista que resulta de la combinación las 
listas aplicando la función f. La función f debe recibir como parámetro un elemento de cada lista a combinar 
por vez. Utilice la siguiente definición de tipo para su función: 
combinarCon :: (a -> b -> c) -> [a] -> [b] -> [c] --}
--combinarConLC :: (a -> b -> c) -> [a] -> [b] -> [c]
-- combinarCon (\x -> x>2) [1,2,3,4,5] [3,5]
{--combinarCon f xs []
    | null xs = []
    | f (head xs) = (head xs): combinar f (tail xs) []
    | otherwise = combinar f (tail xs) []
combinarCon f [] ys
    | null ys = []
    | f (head ys) = (head ys): combinar f (tail ys) []
    | otherwise = combinar f (tail ys) []
combinarCon f (y:ys) (x:xs) =
    if f x then x:combinarCon f ys xs
    else if f y then y:combinarCon f ys xs
    else combinarCon f ys xs
--}
{-- #i : que reciba un predicado y una lista y luego regresa la lista de elementos que satisfacen el 
predicado. La signatura de la función debería ser:
filtrarLista :: (a -> Bool) -> [a] -> [a] 
i. Realice una versión con guards
ii. Realice una versión con list comprehension --}

filtrarListaG :: (a -> Bool) -> [a] -> [a]
filtrarListaG _ [] = []
filtrarListaG f (x:xs)
    | f x = x : filtrarListaG f xs
    | otherwise = filtrarListaG f xs 

filtrarListaG2 :: (a -> Bool) -> [a] -> [a]
filtrarListaG2 f xs
    | null xs = []
    | f (head xs) = (head xs) : filtrarListaG2 f (tail xs)
    | otherwise = filtrarListaG2 f (tail xs) 

filtrarListaLC :: (a -> Bool) -> [a] -> [a]
filtrarListaLC f lc = [c | c <- lc, f c]

funquiz h a xs 
    | null xs = []
    | a <=0 = []
    | otherwise = h (head xs) : funquiz h (a-1) (tail xs)

--f (\a ->a-1) 4 [1,2,3,4,5]
-- PRACTICAS SIMPLES
suma x y = x + y
-- 111 `mod` 111

mastra xs a b = [z | z<-xs, a<= z &&  z<=  b]
mostro xs a b = [1 | z<-xs, a<= z &&  z<=  b]
maestro xs a b = sum [1 | z<-xs, a<= z &&  z<=  b]

-- Composicion: solo las tenemos que definir en consola
g x = x^2 
f x = x + 1 
fg = f . g

f1 x = x + 1

misterio4 f n xs 
    | null xs = []
    | n <=0 = xs
    | f (head xs) = misterio4 f  (n-1) (tail xs)
    | otherwise = f (head xs) :  misterio4 f  n (tail xs)

misterio5 f n xs 
    | null xs = []
    | n <=0 = []
    | f (head xs) = f (head xs) : misterio5 f  (n-1) (tail xs)
    | otherwise =  misterio5 f  n (tail xs)