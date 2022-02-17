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




(*Exercice 2.12*)
(*2.12.1/*)

(*Q1/*)
(*int_of_char(8);;*) (*This doesn't work because an expression of type char was expected but we gave an int and the literal name is an int out of a char so we needed a character*)
int_of_char(’8’);; (*This time 8 is a character because we putted the '' which defines a character and it then gives us 56 which is the ASCII code of 8*)
int_of_char(’a’);; (*a is a character and it's value in ASCII is 97*)
int_of_char(’A’);; (*A is here also a character but this time it's value is 65 so we can see that there is a difference in terms of value for upper case and lower case letters in ASCII*)

(*Q2/*)
char_of_int(-5);; (*This returns an error simply because all ASCII codes are between 0 and 255 (for the extend one otherwise it's between 0 and 127) and we gave -5 which is not in the range so we have an "invalid argument" error*)
char_of_int(33);; (*This time it works because we are in the range said above and it gives us the character '!'*)
char_of_int(255);; (*It gives us '\255' in OCaml but it is actually ÿ in ASCII it's just that apparently OCaml doesn't know how to write ÿ and when we try int_of_char('ÿ') it doesn't work even though it should be defined*)
char_of_int(256);; (*As expected, this gives an error of type "invalid argument" because it is not in the range of the ASCII codes*)
(*Since we know that the maximum is 255 then k is 8 in 2^(k)-1 because it gives 255*)

(*2.12.2/*)
(*Q3/*)
(* Specification for chiffreVbase10 :
   Profile = chiffreVbase10: chiffre -> base10
   Semantic = Takes an integer and writes in the form of a base 10 number.*)

(*Q4/*)
(* Specification for baseVchiffre10 :
   Profile = chiffreVbase10: base10 -> chiffre
   Semantic = Takes an element of the base 10 and writes it as an integer.*)

(*Q5/*)
(*Realisation of chiffreVbase10:
  Since in ASCII code numbers follow each others (0 in ASCII is 48, 1 is 49 and so on until 9 because we just need those to construct other numbers) 
  then we would need to add the number to 0 (whcih would give us 0 in our way of counting) but do it with ASCII code so add the number to the ASCII code of 0 
  which will give us the ASCII code of the number wanted and then use char_of int of this sum to compute the character of the number which will be the number in base10*)
(*Realisation of base10Vchiffre:
  Since in ASCII code numbers follow each others (0 in ASCII is 48, 1 is 49 and so on until 9 because we just need those to construct other numbers) 
  then if we do the difference the ASCII code number of the number with the ASCII code 0 it simply gives the number itself so we simply need to do the difference
  by using the function int_of_char that will give the element in base 10 into an integer which corresponds to the character of the number in ASCII for the wanted number and 0
  and the function (-) to do the difference and compute the number*)

(*Q6/*)
type base10 = char;; (*Since the numbers in the base are character then we say that the type base10 is the same type of character*)
type number = int;; (*Since a number is an integer then we say that the type number is the same as int*)
  
(*Creating the function as we said in question 5*)
let base10Vchiffre (a:base10):number= (*/!\ a as to be between '0' and '9'*)
  int_of_char(a) - int_of_char(’0’) ;;
  
let chiffreVbase10 (b:number):base10= (*/!\ b as to be between 0 and 9*)
  char_of_int(b + int_of_char(’0’)) ;;

assert((chiffreVbase10 0) = '0');;
assert((chiffreVbase10 9) = '9');;

assert((base10Vchiffre '0') = 0);;
assert((base10Vchiffre '9') = 9);;
(*We can see that the tests worked*)

(*Exercice 2.13*)
(*2.13.1/*)

(*Q1/*)
(* Specification for carhexVbase16 :
   Profile = carhexVbase16: carhex -> base16
   Semantic = Takes an character that is either a letter or a number and writes it as an integer in base 16.*)

(*Q2/*)
type carhex = char (*The type charex is the reunion of a number as a character between 0 and 9 and a character from the 6 first letter of the alphabet 
                    so it's a char /!\ the int as to be between 0 and 9 and the letter an upper case of the 6 first letter of the alphabet*)
type base16 = int (*Since a number in base16 is an integer then we say that the type base is the same as int*)

(*Q3/*)
(*Realisation:
  Since we have either a character as a character or a number as a character then we have to check in which case we are by using comparisons with int_of_char('A') and int_of_char('F')
  to see if the character is a character otherwise if it's False then it's a number and if it's a character we take the number in ASCII of the letter in the alphabet substract by
  the number in ASCII code of A which is the first letter of the upper case alphabet and add 1 to this substraction (because otherwise it will give the position of the previous
  letter in the alpahbet) and then we add 9 to this postion to get it in base16. If it's a number then just use the function base10Vchiffre(a) which will give us the 
  character of the number as it's own number which is what we want.
  *)
let carhexVbase16(a:carhex):base16=
  if int_of_char('A')<=int_of_char(a) && int_of_char(a)<=int_of_char('F') then (int_of_char(a)-int_of_char('A')+1)+9 else base10Vchiffre(a);;
carhexVbase16 ('0');;
(*Do asserts*)
(*Q4/*)
