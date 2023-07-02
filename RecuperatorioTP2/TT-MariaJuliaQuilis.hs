--ejercicio 1:

--a)

sumaDivisores :: Int -> Int -> Int
sumaDivisores _ 0 = 0
sumaDivisores n k | mod n k == 0 = k + sumDiv 
                  | otherwise = sumDiv 
				  where sumDiv = sumaDivisores n (k-1)
				  
sumaDeDivisoresPropios :: Int -> Int 
sumaDeDivisoresPropios 0 = 0
sumaDeDivisoresPropios n = sumaDivisores n (n-1)

--b)

esPerfecto :: Int -> Bool
esPerfecto n = n == sumaDeDivisoresPropios n

--ejercicio 2:

--a)

listaAlicuotaDeNDeLargo :: Int -> Int -> [Int]
listaAlicuotaDeNDeLargo 0 _ = []
listaAlicuotaDeNDeLargo k n = n:(listaAlicuotaDeNDeLargo (k-1) (sumaDeDivisoresPropios n))

--b)

-- armo una función que determina si los números que están en esa lista son distintos.

ultimoElemento :: [Int] -> Int
ultimoElemento xs | tail xs == [] = head xs
                  | otherwise = ultimoElemento (tail xs)

pertenece :: Int -> [Int] -> Bool
pertenece _ []     = False
pertenece c (x:xs) = c == x || pertenece c xs

sondistintos :: [Int] -> Bool
sondistintos [] = True
sondistintos (x:xs) | pertenece x xs = False
                    | otherwise = sondistintos xs  

--armo una función que determina si la suma de divisores propios del k-ésimo elemento de una lista es igual al (k+1)-ésimo elemento de la lista.

sumaDeDivisoresPropiosDeCualquierK :: [Int] -> Bool
sumaDeDivisoresPropiosDeCualquierK [] = False
sumaDeDivisoresPropiosDeCualquierK [x] = True
sumaDeDivisoresPropiosDeCualquierK (x:xs) = (sumaDeDivisoresPropios x == head xs) && sumaDeDivisoresPropiosDeCualquierK xs


sonSociables :: [Int] -> Bool
sonSociables [x] = esPerfecto x 
sonSociables (x:xs) = sondistintos (x:xs) && (sumaDeDivisoresPropiosDeCualquierK (x:xs)) && (sumaDeDivisoresPropios (ultimoElemento xs)) == x


--ejercicio 3:

--a)

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas n (x:xs) | n == x = quitTodas 
                     | otherwise = x:quitTodas
                     where quitTodas = quitarTodas n xs

eliminarRepetidos :: [Int] -> [Int]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x:eliminarRepetidos (quitarTodas x xs)


minimoElemento :: [Int] -> Int
minimoElemento [] = 0
minimoElemento [x] = x 
minimoElemento (x:xs) = minimo x (minimoElemento xs)

minimo :: Int -> Int -> Int
minimo a b | a >= b = b 
           | otherwise = a

minimosDeKClubesMenoresQue' :: Int -> Int -> [Int]
minimosDeKClubesMenoresQue' _ 0 = []
minimosDeKClubesMenoresQue' k c | sonSociables(listaLargo) = (minimoElemento(listaLargo)):(listaMinimo)
                                | otherwise = listaMinimo
                                where listaLargo = listaAlicuotaDeNDeLargo k c 
                                      listaMinimo = minimosDeKClubesMenoresQue' k (c-1)


minimosDeKClubesMenoresQue :: Int -> Int -> [Int]
minimosDeKClubesMenoresQue k c = eliminarRepetidos(minimosDeKClubesMenoresQue' k c)


--b)

--armo una función que devuelve el máximo elemento de una lista.

maximo :: Int -> Int -> Int
maximo a b | a >= b = a
           | otherwise = b

maximoLista :: [Int] -> Int
maximoLista [] = 0
maximoLista [x] = x 
maximoLista (x:xs) = maximo x (maximoLista xs)

--armo una función que devuelve la lista de n clubes con números mayores que a y menores que k.

listaDeNClubesConNrosMenoresDesdeAHastaK :: Int -> Int -> Int -> [[Int]]
listaDeNClubesConNrosMenoresDesdeAHastaK a k n | a > k = []
                                               | sonSociables(listaAlicuota) && (a == minimoElemento(listaAlicuota)) && (maximoLista(listaAlicuota) < k) = (listaAlicuota):(listaClubes)
                                               | otherwise = listaClubes
                                               where listaAlicuota = listaAlicuotaDeNDeLargo n a 
                                                     listaClubes = listaDeNClubesConNrosMenoresDesdeAHastaK (a+1) k n 


listaDeNClubesConNrosMenoresQue :: Int -> Int -> [[Int]]
listaDeNClubesConNrosMenoresQue n k = listaDeNClubesConNrosMenoresDesdeAHastaK 1 k n


--ejercicio 4:

--para la función indiceDeAbundancia no puse qué pasa cuando n = 0 porque n es un número natural.

indiceDeAbundancia :: Int -> Float
indiceDeAbundancia n = fromIntegral(sumaDivisores n n)/fromIntegral n 

sonAmigables :: Int -> Int -> Bool
sonAmigables n m = (indiceDeAbundancia n == indiceDeAbundancia m) && (n /= m) 

esAmistosoEn :: Int -> [Int] -> Bool
esAmistosoEn n [] = False
esAmistosoEn n (x:xs) = sonAmigables n x || esAmistosoEn n xs 


