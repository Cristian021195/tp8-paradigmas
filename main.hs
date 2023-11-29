-- TRABAJO PRACTICO N°8
-- PUNTO #1: Explique que hace "misterio" y de un nombre mas adecuado
misterio x y
    | (y==0 || x==0) = 0
    | x<0 && y<0 = misterio (-x) (-y)
    | y<0 = misterio y x
    | otherwise = x + misterio x (y-1)

multiplicacion x y
    | (y==0 || x==0) = 0
    | x<0 && y<0 = misterio (-x) (-y)
    | y<0 = misterio y x
    | otherwise = x + misterio x (y-1)

-- PUNTO #2: Implemente en haskell las siguientes funciones
-- #a: 
prodss x y
    | (x == 0 || y == 0) = 0
    | x > 0 = x + prodss x (y-1)
    | y > 0 = x + prodss (x-1) y

-- #b:
sumadig x
    | (x <= 0) = 0
    | (x >= 1 && x < 10) = x
    | otherwise = (x `mod` 10) + sumadig(x/10)

-- #c:
contariguales x ys = length[i | i <- ys, x == i]

contarigualesalt [] y = 0
contarigualesalt (x:xs) y = if y==x then 1 + contarigualesalt xs y else contarigualesalt xs y

contarigualesalt2 [] y = 0
contarigualesalt2 (x:xs) y = (if y==x then 1  else 0) + contarigualesalt xs y

-- #d:
eliminariguales x ys= [i | i <- ys, x /= i]

-- #e:
listarNaturalesGuards x
    | (x <= 1) = [1]
    | (x > 1) = [x] ++ listarNaturalesGuards(x-1)

listarNaturalesPM 0 = []
listarNaturalesPM 1 = [1] 
listarNaturalesPM n = [n] ++ listarNaturalesPM(n-1)

listarNaturalesLC n = [x | x <- [1..n]]

-- #f
sublista x ys = [i | i <- ys, x < i]

-- #g

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

