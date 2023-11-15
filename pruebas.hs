module Test where

division :: Int -> Int -> Int
division x y
    | (x /=0 && y == 0) = 0
    | otherwise = x `div` y

divisionf :: Float -> Float -> Float
divisionf x y
    | (x /=0 && y == 0) = 0
    | otherwise = x / y

cuadrado :: Int -> Int
cuadrado x = x * x


multiplicacion :: Int -> Int -> Int
multiplicacion x y
    | (x == 0 || y == 0) = 0
    | otherwise = x*y

especial x y z
    | (y == 0 || z == 0) = x
    | otherwise = x + (y*z)

{-
    Tipos de funciones en Haskell
    - Prefija: antes del argumento/operando, puede ser mas de uno, ej -(-2), `div`
    - Infija: almenos de 2 argumentos/va en medio, por ej 2 + 2
    - Posfija: Se escribe despues de los operandos, ej: 2 cuadrado
    nota: en el caso de division, podemos usar prefija e infija
    nota: los modulos comienzan con mayusculas 'Test' where, donde tiene nuestras fnes
    nota: la aplicacion de funciones, tiene mas prioridad que otras operaciones matematicas
        xej: f a b + c * d, primero f ab, luego c*d luego +
    equivalencias matematicas: todas asocian mirando a la izquierda
        f(x) = f x
        f(x,y) = f x y
        f(g(x)) = f (g x)
        f(x, g(y)) = f x (g y)
        f(x)g(y) = f x * g y
    nota:
        t: suma, es la funcion con retorno de tipo de dato
    nota: declaracion y definicion en cmd linea
        suma :: Int -> Int -> Int; suma x y = x + y
    nota: composicion en cmd
        - especial (multiplicacion 2 2) 2 2
        - especial (cuadrado 2) 2 2, etc..
    
-}