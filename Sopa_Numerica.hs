--Samantha Sol Zelada LU:489/16
data Desplazamiento = Arriba | Abajo | Izquierda | Derecha deriving (Show, Eq)

type Posicion = (Integer,Integer)
type Tablero a = [[a]]
type SopaNumerica = Tablero Integer

-- Devuelve el tamaño de un tablero.
tamano :: Tablero a -> Integer
tamano t = fromIntegral(length t)

-- Devuelve el valor de una posición de un tablero.
-- Notar que la primera posición de arriba a la izquierda es la (1,1).
valor :: Tablero a -> Posicion -> a
valor t (i,j) = iesimo (iesimo t i) j

-- Devuelve el iésimo elemento de una lista. El primer elemento es el 1.
iesimo :: [a] -> Integer -> a
iesimo (x:xs) 1 = x
iesimo (x:xs) n = iesimo xs (n-1)

-- Determina si una posición está dentro de los límites de un tablero.
posValida :: Tablero a -> Posicion -> Bool
posValida t (i,j) = 1<=i && i<=n && 1<=j && j<=n
    where n = tamano t

-- Sopa Numérica de prueba.
test_sopa1 :: SopaNumerica
test_sopa1 = [ [1, 2, 3],
               [4, 5, 6],
               [7, 8, 9] ]

               --Recuperatorio del trabajo practico

--Dado un numero n devuelve una lista de los digitos de n
digitos :: Integer -> [Integer]
digitos 0 = []
digitos n | n<10 = [n]
digitos 10 = [10]
digitos n = digitos (n `div` 10) ++ [n `mod` 10]

-- Funcion auxiliar para numerosEnsSopa (usa listas)
-- Si una posicion no es valida -> False
-- Si el valor de la posicion no corresponde con x -> false
-- Por lo tanto para toda posicion de una sopa t determina si se puede armar la lista
digitosEnSopa :: SopaNumerica -> Posicion -> [Integer] -> Bool
digitosEnSopa t (i,j) (x:xs) | posValida t (i,j) == False = False
digitosEnSopa t (i,j) (x:xs) | valor t (i,j) /= x = False
digitosEnSopa t (i,j) [] = posValida t (i,j)
digitosEnSopa t (i,j) (x:xs) = valor t (i,j) == x && digitosEnSopa t (i+1,j) xs || digitosEnSopa t (i-1,j) xs || digitosEnSopa t (i,j-1) xs || digitosEnSopa t (i,j+1) xs

numeroEnSopa :: SopaNumerica -> Posicion -> Integer -> Bool
numeroEnSopa t (i,j) n = digitosEnSopa t (i,j) (digitos n)
