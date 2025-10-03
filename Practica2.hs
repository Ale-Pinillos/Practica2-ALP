contar :: Eq a => a -> [a] -> Int -- El funcionamiento de la función; recibe 2 parámetros: un elemento y una lista, y devuelve un entero.

{-
Inicio de la función a la que llamé "contar". Cuando la lista está vacía, devuelve 0. El guión bajo indica que no nos importa el
valor del primer parámetro en este caso.
-}
contar _ [] = 0 
contar elemento (c:l) = -- "elemento" es lo que deseamos contar, "c" es la cabeza de la lista y "l" es la cola de la lista.
    if elemento == c  -- Si el elemento que queremos contar es igual a la cabeza de la lista...
        then 1 + contar elemento l -- ...entonces devolvemos 1 + el resultado de contar el elemento en la cola de la lista.
        else contar elemento l -- Si no son iguales, aplicamos la función a la cola de la lista sin sumar 1.

{- 
Acá evidenciamos la recursividad en las líneas 11 y 12, ya que utilizamos a la propia función "contar" dentro de su definición.
-}

listaEjemplo = [3, 1, 5, 2, 4, 1, 3, 2, 5, 4, 2, 1, 5, 3, 4, 2, 1, 5, 3, 4]

main = do -- Agregamos la función main para ejecutar el código.
    let a = 'a' -- Definimos el elemento que queremos contar.
    print (contar a "parangaricutirimicuaro") -- Llamamos a la función contar para contar las 'a' en "parangaricutirimicuaro".

    let b = 1 -- Definimos un elemento de otro tipo y que queremos contar para confirmar que funciona independientemente del tipo de dato.
    print (contar b listaEjemplo) -- Llamamos a la función de nuevo para contar los 1 en la listaEjemplo.

    let c = 'o' 
    print (contar c "otorrinolaringólogo") 

-- 3- Cómo definirias or y and en terminos de foldr?
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

         | p x = True --Si x esta dentro de la funcion entonces es verdadera
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

