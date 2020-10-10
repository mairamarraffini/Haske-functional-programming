# PRÁCTICA 6 

## Sinónimos de tipos. Tipos algebraicos recursivos. Árboles.
 
1. Definir las operaciones de **unión** e **intersección** de dos conjuntos y el predicado de pertenencia
para conjuntos de elementos de tipo a, los cuales se representan:

a) por extensión (como lista de elementos de a).

b) por comprensión (como predicado `a ->Bool`);

> Utilizamos el tipo de dato `Set e`. Representa un conjunto de elemetos de tipo `e`. Se requiere que `e` sea una instancia de la clase `Ord`.
> `Set` permite almacenar elementos *únicos* y *ordenados*. 

Para diferenciar:

- There is a `Set data type`.
- `Set` is not part of the *language specification*, and it is not in the *Prelude*.
- This is because the language is powerful enough to express the idea of a set without needing to bake it in.


2. `data TipTree a = Tip a | Join (TipTree a) (TipTree a)`



3. ¿Se pueden representar listas ordenadas mediante tipos algebraicos? Proponga una definición o justifique.
