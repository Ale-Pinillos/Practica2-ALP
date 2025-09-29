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