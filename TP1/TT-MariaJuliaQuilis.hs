-- Ejercicio 1)

med :: Float -> Float -> Int -> Float
med i0 b 0 = i0
med i0 b n = med i0 b (n-1) + b * (med i0 b (n-1))


-- Ejercicio 2)

mld :: Float -> Float -> Float -> Int -> Float
mld p i0 b 0 = i0  
mld p i0 b n = inf + b * ((p - inf)/p) * inf
             where inf = mld p i0 b (n-1)


-- Ejercicio 3)

{- Armo tres funciones que calculen la cantidad de individuos sanos, infectados y recuperados. Estas funciones las voy a usar 
   para poder calcular sir.-}


sanos :: Float -> Float -> Float -> Float -> Int -> Float
sanos s0 i0 b g 0 = s0  
sanos s0 i0 b g n = s - b * i * s
                where s = sanos s0 i0 b g (n-1)
                      i = infectados s0 i0 b g (n-1)

infectados :: Float -> Float -> Float -> Float -> Int -> Float
infectados s0 i0 b g 0 = i0
infectados s0 i0 b g n = i + b * i * s - g * i
                     where s = sanos s0 i0 b g (n-1)
                           i = infectados s0 i0 b g (n-1)

recuperados :: Float -> Float -> Float -> Float -> Float -> Int -> Float
recuperados s0 i0 r0 b g 0 = r0 
recuperados s0 i0 r0 b g n = r + g * i
                         where r = recuperados s0 i0 r0 b g (n-1)
                               i = infectados s0 i0 b g (n-1)

sir :: (Float, Float, Float) -> Float -> Float -> Int -> (Float, Float, Float)
sir (s0, i0, r0) b g 0 = (s0, i0, r0)
sir (s0, i0, r0) b g n = (sanos s0 i0 b g n, infectados s0 i0 b g n, recuperados s0 i0 r0 b g n)


-- Ejercicio 4)

-- Armo una función que me devuelva la segunda coordenada de una tupla de tres elementos:

segCoord :: (Float, Float, Float) -> Float
segCoord (a, b, c) = b 

-- Armo una función que, dado dos días llamados n y m, me devuelva la cantidad máxima de infectados entre esos días:

cantMaxInf :: (Float, Float, Float) -> Float -> Float -> Int -> Int -> Float 
cantMaxInf (s0,i0,r0) b g n m | n == m = segCoord (sir (s0, i0, r0) b g n)
                              | segCoord (sir (s0, i0, r0) b g n) > segCoord (sir (s0, i0, r0) b g m) = cantMaxInf (s0,i0,r0) b g n (m+1)
						      | otherwise = cantMaxInf (s0,i0,r0) b g (n-1) m

maxsir :: (Float, Float, Float) -> Float -> Float -> Int -> Float 
maxsir (s0,i0,r0) b g n = cantMaxInf (s0,i0,r0) b g n 0 

