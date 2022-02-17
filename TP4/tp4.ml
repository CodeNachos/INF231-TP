(* -----------------------------------------------------------------------
   inf201-ABDELKADER-MASCARENHAS-DOKTORCIK-TP3.ml : cr exercices TP no4
   Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr>  \ 
   Rafael MASCARENHAS <Rafael.Mascarenhas@etu.univ-grenoble-alpes.fr>   > Groupe Ocaml_best_camel
   Maxence DOKTORCIK <Maxence.Doktorcik@etu.univ-grenoble-alpes.fr>    / 
----------------------------------------------------------------------- *)

(*
  Exercice 2.10.
  2.10.1
  Q1.
*)

(* SPECIFICATION

   Profile        poCoupleE: int * int -> int*int
   Semantics      let (xp, yp) = poCoupleE(x, y) is the ordered permutation of 
                  (x, y), defined as follows :

                  (xp, yp) = {(x, y) if x <= y,
                              (y, x) else}

    Ex and Prop.  (i)  poCoupleE(3,4) = (3,4)
                  (ii) poCoupleE(4,3) = (3,4)
*)

let poCoupleE (a,b:int*int) : (int*int)=
  if a > b then (b,a)
  else (a,b)
;; 

(* function test *)
assert((poCoupleE (3,4)) = (3,4));;
assert((poCoupleE (4,3)) = (3,4));;

(* Q2. *)
(*
poCoupleE (33.3, 14.5);;

Output:
28 | poCoupleE (33.3, 14.5);;
                ^^^^
Error: This expression has type float but an expression was expected of type
         int
*)

(* The function expected a cuple of two integers but a product of two float 
was given producing an error *)

(*
   2.10.2
   Q3.
*)

2 < 3 ;;
(* Output: - : bool = true *)
2.0 < 3.0 ;;
(* Output: - : bool = true *)

(* Both expressions are of boolena type *)

(* Q4 *)

(* OPERATORS OVERLOADING TEST
    
  Structural equality test '=' *)
2 = 3 ;;
(* Output: - : bool = false *)
2.0 = 3.0 ;;
(* Output: - : bool = false *)

(* Structural inequality test '<>'*)
2 <> 3 ;;
(* Output: - : bool = true *)
2.0 <> 3.0 ;;
(* Output: - : bool = true *)

(* Physical equality test '==' *)
2 == 3 ;;
(* Output: - : bool = false *)
2.0 == 3.0 ;;
(* Output: - : bool = false *)

(* Physical inequality test '!=' *)
2 != 3 ;;
(* Output: - : bool = true *)
2.0 != 3.0 ;;
(* Output: - : bool = true *)

(* Less than equal test '<=' *)
2 <= 3 ;;
(* Output: - : bool = true *)
2.0 <= 3.0 ;;
(* Output: - : bool = true *)

(* Greather than test '>' *)
2 > 3 ;;
(* Output: - : bool = false *)
2.0 > 3.0 ;;
(* Output: - : bool = false *)

(* Greater equal than test '>=' *)
2 >= 3 ;;
(* Output: - : bool = false *)
2.0 >= 3.0 ;;
(* Output: - : bool = false *)

(* One can deduce from those tests that all comparison operators are overloaded *)

(* Q5 *)

(* SPECIFICATION

   Profile        poCoupleR: float*float -> float*float
   Semantics      let (xp, yp) = poCoupleE(x, y) is the ordered permutation of 
                  (x, y), defined as follows :

                  (xp, yp) = {(x, y) if x <= y,
                              (y, x) else}
    
    Ex and Prop.  (i)  poCoupleE(3.0,4.0) = (3.0,4.0)
                  (ii) poCoupleE(4.0,3.0) = (3.0,4.0)
*)

let poCoupleR (a,b :float*float) : (float*float)=
  if a > b then (b,a)
  else (a,b)
;;

(* function test *)
assert((poCoupleR (3.0,4.0)) = (3.0,4.0));;
assert((poCoupleR (4.0,3.0)) = (3.0,4.0));;

(*
   2.10.3
   Q6.
*)

let poCouple (x,y: 'un_type*'un_type) : 'un_type*'un_type =
  if x <= y then x,y else y,x
;;
(* Output: 
val poCouple : 'un_type * 'un_type -> 'un_type * 'un_type = <fun> *)

(* Q7. *)

poCouple (3, 2) ;; poCouple (33.3, 14.5) ;;
(* Output:
- : int * int = (2, 3)
- : float * float = (14.5, 33.3) *)

(*poCouple (3, 14.5) ;; 
Output: 
145 | poCouple (3, 14.5) ;;
                   ^^^^
Error: This expression has type float but an expression was expected of type
         int

This expression throw an error. Even if the types are not specified the cuple
must be composed of two elements of the same type
*)

(* Q8. *)

(*
  From the previous questions one can infer that the operators <, <=, >, >= are
  polymorphs

  Profile < 'a -> 'a -> bool
  Profile <= 'a -> 'a -> bool
  Profile > 'a -> 'a -> bool
  Profile >= 'a -> 'a -> bool
*)

(<) ;;
(* Output: - : 'a -> 'a -> bool = <fun> *)
(<=) ;;
(* Output: - : 'a -> 'a -> bool = <fun> *)
(>) ;;
(* Output: - : 'a -> 'a -> bool = <fun> *)
(>=) ;;
(* Output: - : 'a -> 'a -> bool = <fun> *)

(* Q9. *)

poCouple ('a', 'b') ;;
(* Output: - : char * char = ('a', 'b') *)
poCouple ('b', 'a') ;;
(* Output: - : char * char = ('a', 'b') *)
poCouple ('d', '#') ;;
(* Output: - : char * char = ('#', 'd') *)

(* Indeed the order follows the one of the characters code *)



