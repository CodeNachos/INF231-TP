(*EXERCICE 1.2 9
somme3 : int -> int -> int -> int 10
Sémantique : somme de trois entiers 11
Algorithme : utilisation de + 12
*)
let somme3 (a:int) (b:int) (c:int) : int = 
a + b + c 
(* Tests : *) 
let - = assert ((somme3 1 2 3) = 6) (* cas général *) 
let - = assert ((somme3 (-1) 2 (-1)) = 0) (* entiers négatifs *) 
let - = assert ((somme3 0 0 0) = 0) (* cas zéro *)
