-- Ejercicio 1.I: 

{- Inicio de la función "contar". Cuando la lista está vacía, devuelve 0. El guión bajo indica que no nos importa el
valor del primer parámetro en este caso.-}

contar _ [] = 0 -- Caso base: Si la lista está vacía, devolvemos 0.
contar elemento (c:l) = -- "elemento" es lo que deseamos contar, "c" es la cabeza y "l" es la cola de la lista.
    if elemento == c  -- Caso recursivo: Si el elemento que queremos contar es igual a la cabeza...
        then 1 + contar elemento l -- ... devolvemos 1 + el resultado de contar el elemento en la cola de la lista.
        else contar elemento l -- Si no son iguales, aplicamos la función a la cola sin sumar 1.

-- Acá evidenciamos la recursividad en las líneas 11 y 12, ya que utilizamos a la propia función "contar" dentro de su definición.

-- Ejercicio 1.II:
eliminar _ [] = [] -- Caso base: Si la lista está vacía, devolvemos la misma lista vacía.
eliminar e (x:xs) = -- Queremos eliminar a "e" de la lista.
    if e == x -- Caso recursivo: Si "e" es igual a la cabeza...
        then eliminar e xs -- ... aplicamos la función solo a la cola.
        else x : eliminar e xs -- Si son diferentes, mantenemos la cabeza y aplicamos la función a la cola.

-- Asignamos valores a variables para posteriormente ponerlas a prueba en la función main.
listaEjemplo = [3, 1, 5, 2, 4, 1, 3, 2, 5, 4, 2, 1, 5, 3, 4, 2, 1, 5, 3, 4]
a = 'a' 
b = 1
c = 'o'
word1 = "parangaricutirimicuaro"
word2 = "otorrinolaringologo" -- Sin tilde porque luego me da error jajan't.

--EJERCICIO 2: Define las siguientes dos funciones.

--1) Una función que tome dos listas y devuelva True si todos los elementos de la primera lista también aparecen en la otra.

esSubconjunto :: Eq a => [a] -> [a] -> Bool
esSubconjunto [] _ = True --Caso Base
--Si la primera lista está vacía, consideramos que es un subconjunto de cualquier otra lista,
por lo que devolvemos True. El _ significa que no nos importa cuál sea la segunda lista.
esSubconjunto (x:xs) ys = (elem x ys) && esSubconjunto xs ys --Paso Recursivo
--Descompone la primera lista en su primer elemento (x) y el resto (xs).
--Verifica si x está en la segunda lista (ys) usando la función elem.
--Usa el operador && (Y lógico) para combinar ese resultado con el resultado de la llamada recursiva
--Subconjunto xs ys, que hace exactamente lo mismo para el resto de la lista.

--2) Una función que tome dos listas y devuelva una lista con los valores que aparecen en ambas listas.

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] _ = [] --Caso Base: La intersección con una lista vacía es vacía
interseccion (x:xs) ys --Caso Recursivo (Comienza cuaando la primera lista no es vacia)
  | elem x ys = x: interseccion xs ys --Si x está en ys, se guarda
  | otherwise = interseccion xs ys --Si no, se descarta

-- Ejercicio 3:- Cómo definirias or y and en terminos de foldr?

-- Or siempre nos devuelve true si al menos un elemento es verdad 
-- And nos devuelve true si todos sus elementos son verdad
-- Entonces teniendo eso claro:
or' :: [Bool] -> Bool -- Se declara la función tipo lista y regresamos un booleano 
or' = foldr (||) False 
and' :: [Bool] -> Bool -- Declaramos la lista 
and' = foldr (&&) True

--Ejercicio 4 : Define tu propia versión (recursiva) de any y all.

--a)Para Any:

miAny :: ( a -> Bool ) -> [ a ] -> Bool --Firma de nuestra funcion

miAny _ [] = False --Caso base
--El guion bajo _ significa que no importa el primer argumento de la funcion
--Se devuelve False por que no hay nada dentro de la lista entonces no cumple con la firma de la funcion

miAny p (x:xs) = p x || miAny p xs --Caso Recursivo
--Esta lista ya no es vacia
--Es un caso Recursivo por que se vuelve a llamar asi misma pero con la cola de la lista xs, hasta llegar al caso base 

         | p x = True -- Si x esta dentro de la funcion entonces es verdadera
--Se usa || entonces lo que se agrege despues de esto sera True
         | = False || (False || (False || False))  --Si ningun elemento cumple con la funcion es False

--b)Para All:

miAll :: (a -> Bool) -> [a] -> Bool --Firma de nuestra funcion

miAll _ []     = True --Caso base
--El guion bajo _ significa que no importa el primer argumento de la funcion
--Este es un conjunto vacio por lo tanto es verdadera ya que no hay nada que pueda contradecirlo

miAll p (x:xs) = p x && miAll p xs --Caso Recursivo
--Esta lista ya no es vacia
--Es un caso Recursivo por que se vuelve a llamar asi misma pero con la cola de la lista xs, hasta llegar al caso base

         | miAll p xs = True --Si todos los elementos de la lista cumplen la funcion entonces es True
         | = True   && (False  && ...) --Si al menos hay un elemento que no cumpla con la funcion entonces es False

--Ejercicio 5: 

--- Para Any en términos de map y or
miAny :: (a -> Bool) -> [a] -> Bool
miAny p xs = or (map p xs)

-- Para All en términos de map y and
miAll :: (a -> Bool) -> [a] -> Bool
miAll p xs = and (map p xs)

--Ejercicio 6:

-- Para la función takeWhile recursiva
miTakeWhile :: (a -> Bool) -> [a] -> [a]
miTakeWhile _ [] = []
miTakeWhile p (x:xs)
    | p x       = x : miTakeWhile p xs
    | otherwise = []

-- Para la función dropWhile recursiva
miDropWhile :: (a -> Bool) -> [a] -> [a]
miDropWhile _ [] = []
miDropWhile p (x:xs)
    | p x       = miDropWhile p xs
    | otherwise = x:xs

main = do -- Agregamos la función main para ejecutar el código.
-- 1.I:
    print (contar a word1) -- Llamamos a la función.
    print (contar b listaEjemplo) -- Probamos la función con otro tipo de dato.
    print (contar c word2) 

-- 1.II:
    print (eliminar a word1) -- Invocamos a la función eleminar.
    print (eliminar b listaEjemplo) -- Corroboramos que funciona independientemente del tipo de dato.
    print (eliminar c word2)