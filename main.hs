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
diferencia :: [Int] -> [Int] -> [Int]
diferencia [] _ = []
diferencia _ [] = []
diferencia (x:xs) (y:ys)
    | 

existe :: [Int] -> Int -> Bool
existe [] _ = False
existe (x:xs) y
    | x == y = True
    | otherwise = existe xs y


-- #h
transformar :: (a -> b) -> [a] -> [b] 
transformar _ [] = [] 
transformar f (x:xs) = f x : transformar f xs

-- #i
tablaDePares n = [2*x | x <- [0..n/2]]

-- #j - pendiente
verificar [] f = True
verificar (x:xs) f = if f x then verificar xs f else False

-- #k

-- #i


-- PRACTICAS SIMPLES
suma x y = x + y
-- 111 `mod` 111

-- Composicion
g x = x^2 
f x = x + 1 
fg = f . g

