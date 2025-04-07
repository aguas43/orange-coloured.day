doble x = x + x
-- prob 1
-- f :: int -> int
-- problema f (n : Z) : Z {
-- requiere: {n = 1 ∨ n = 4 ∨ n = 16}
-- asegura: {(n = 1 → res = 8) ∧ (n = 4 → res = 131) ∧ (n = 16 → res = 16)}
-- }
f 1 = 8
f 4 = 131
f 16 = 16
g 8 = 16
g 16 = 4
g 131 = 1
h n = f(g(n))
k a = g(f(a))
-- prob 2
-- a) absoluto: calcula el valor absoluto de un numero entero.
absoluto x = abs x 
-- b) maximoAbsoluto: devuelve el m´aximo entre el valor absoluto de dos n´umeros enteros.
maximoAbsoluto x y | abs(x) >= abs(y) = x
maximoAbsoluto _ y = y
-- c) maximo3: devuelve el m´aximo entre tres n´umeros enteros.
maximo3 x y z | (x >= y) && (x >= z) = x
              | (y >= x) && (y >= z) = y
              | otherwise = z
-- preguntar maximo3 x y z | (x >= y) && ( x >= z) = x
--                         | otherwise = maximo3 (z x y)
--d) algunoEsCero: dados dos n´umeros racionales, decide si alguno es igual a 0 (resolverlo con y sin pattern matching).
algunoEsCeroSinPatMat x y | x == 0 = x
                          | otherwise = y
algunoEsCeroConPatMat 0 _ = 0
algunoEsCeroConPatMat _ 0 = 0
-- e) ambosSonCero: dados dos n´umeros racionales, decide si ambos son iguales a 0 (resolverlo con y sin pattern matching).
ambosSonCero x y | (x == 0) && (y == 0) = True
                 |otherwise = False
ambosSonCeroConPatMat 0 0 = True
ambosSonCeroConPatMat _ _ = False
-- f) enMismoIntervalo: dados dos n´umeros reales, indica si est´an relacionados por la relaci´on de equivalencia en R cuyas
-- clases de equivalencia son: (−∞, 3],(3, 7] y (7, ∞), o dicho de otra manera, si pertenecen al mismo intervalo.
enMismoIntervalo x y | (x <= 3) && ( y <= 3) = True
                     | (x > 7) && ( y > 7) = True
                     | (x <= 7) && (x > 3) && (y <= 7) && (y > 3) = True
                     |otherwise = False