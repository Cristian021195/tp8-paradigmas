--suma :: Integer -> Integer -> Integer
suma x y = x + y

{--describeLista :: [a] -> String 
describeLista [] = "Lista vacia" 
describeLista [x] = "Lista con un elemento" 
describeLista (x:_) = "Lista con mas de un elemento"
-}
{- main :: IO ()
main = do
    let var1 = 2
    let var2 = 2
    let uno_diez = [1..10]
    -- print(var1+var2)
    -- print([1..10])
    
    -- print(suma 2 3)
    -- print( uno_diez )
    let x = 101
    let a = (if x > 100 then x else x*2)+2
    -- print(a)

-}    

--describeLista [1..2]

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
{-
    a = 3
    b = 2
    b>=0?
    suma = suma + a
    b = b-1
-}
prodss x y
    | (x == 0 || y == 0) = 0
    | x > 0 = x + prodss x (y-1)
    | y > 0 = x + prodss (x-1) y

{-
    suma dig 111
    suma dig
-}
-- sumadig x = 111 `mod` 111
sumadig x
    | (x <= 0) = 0
    | (x >= 1 && x < 10) = x
    | otherwise = (x `mod` 10) + sumadig(x/10)

-- contariguales = [i | i <- [1,2,3,1,5,4,6], 1 == i]
contariguales x ys = length[i | i <- ys, x == i]
contarigualesalt [] y = 0
contarigualesalt (x:xs) y = if y==x then 1 + contarigualesalt xs y else contarigualesalt xs y

contarigualesalt2 [] y = 0
contarigualesalt2 (x:xs) y = (if y==x then 1  else 0) + contarigualesalt xs y

eliminariguales x ys= [i | i <- ys, x /= i]

{-listaNaturalesA x = [1..x]

listaNaturalesG x
    | (x <= 0) = [0]
    | (x >= 1) = [0..x]

listaNaturalesPM 0 = [0]
listaNaturalesPM x = [0..x]
-}
listarNaturalesGuards x
    | (x < 1) = []
    | (x >= 1) = [x] ++ listarNaturalesGuards (x-1)

listarNaturalesPM 0 = []

listarNaturalesGuardsAlt x
    | (x < 1) = []
    | (x >= 1) = [x]listarNaturalesGuards (x-1)

-- sublista x y = [i | i <- y, x < i]

-- diferencia = [1,2,3]
-- (x:xs) y = if y==x then True else pertenece xs y















