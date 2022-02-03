(* -----------------------------------------------------------------------
   inf201-ABDELKADER-DOKTORCIK-MASCARENHAS-TP2.ml : cr exercices TP no2
   Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr>     \
   Maxcence DOKTORCIK <Maxcence.Doktorcik@etu.univ-grenoble-alpes.fr>      > Groupe Ocaml_best_camel
   Rafael MASCARENHAS Couillet <Rafael.Mascarenhas@etu.grenoble-alpes.fr> /
   --------------------------------------------------------------------------*)

(*EXERCICE 2.4*)

(*Q1/*)
(*We define the variables*)
let a:int = 10;; 
let b:int = 0 ;;
(b <> 0) && (a mod b=0);; (*This gives a boolean which is false because && checks the boolean on the left first and if it's false then it return false because of the table of truth*)
(* (a mod b=0) && (b <> 0);; *) (*This gives an error because it first check the boolean on the left but since b=0 a mod b can't exists since it's a division by 0 so it returns an error*)
(*The rule for && is that it checks the boolean on the left, if it's true it compares it to the one on the right, if it's false it returns false without even checking the right one and if it's an error it show the error without checking the right one*)

(*Q2/*)
(*We define the function monExpression*)
let monExpression (a:int)(b:int):bool= 
  (b<>0) && (a mod b=0);; (*This evaluate the two booleans and compares them to be able to see if it's true or false in the table of truth*)
monExpression 10 0 ;;(*Evaluates the function monExpression with the parameters being a=10 and b=0*)
(*We can see that it does the same thing as before because in the function it checks if b is 0 first*)

(*Q3/*)
(*We define the function monEt*)
let monEt (x:bool) (y:bool):bool=
  x && y;; (*Evaluate x and y and returns then the boolean based on the table of truth*)
monEt true false ;; (*This returns false because it checks first the left object which is true so it checks the one on the right but it's false so by the table it's false*)
monEt false true ;; (*This returns false because it checks first the left object which is false so it just returns false as said earlier*)
(* monEt (b <> 0) (a mod b = 0) ;; *) (*This raises an error of type : Exception : Division_by_zero that we are going to explain just below int the conclusion*)
(*Conclusion*)
(*We can conclude that when we call a function, it first evaluates the parameters that we putted before entering the function so if there is an error in one of the paramater
  ,which is our case with monEt (b <> 0) (a mod b=0) because a mod b raises an error, OCaml will just say there is an error and won't execute the function *)



(* EXERCICE : 2.5 Moyenne de deux entiers*)


(*2.5.1 Types numériques:entiers (int) et réels (float) *)

(*Q.1*)
3.5;; (*For '3.5', we get that the type of this value is a float*)
3,5;; (*For '3,5', we get that the type of this value is an int*int, so a tuple essentially.*)
(4+3)/2;; (*We obtain 3 as a result, which is of type int*)

(*Specification for the operator (+):
  Profil = (+) : int -> int -> int
  Semantique = This function computes the sum between two integers, and returns an int which is the
  result of their sum.
  Examples :
            (+) 2 3 = 5
            (+) -2 5 = 3
            (+) 0 8 = 8 *)

(*Specification for the operator (/):
  Profile = (/) : int -> int -> int
  Semantique = This function computes the division between two integers, and returns an int which is
  the floor of the result of the division.
  Examples=
            (/) 6/3 = 2
            (/) 7/2 = 3
            (/) 0/4 = 0 *)

(*Q.2*)

(*(4+3) /.2;;*) (*Error: Ocaml expected only floats instead of integers due to the operator (/.)*)
(*(4. + 3.) /.2;;*) (*Error: Ocaml expected only integers instead of floats due to the operator (+) *)
(*(4.0 +. 3.0) /.2;;*) (*Error: Ocaml expected only floats instead of integers due to the operators (+) and 
(/) *)

(4.0 +. 3.0) /.2. ;; 
(*This operation functions properly because all operators and values are of the
same type *)

(* Specification for (+.):
   Profile = (+.): float -> float -> float
   Semantique = Computes the sum between two floats, and returns a single float which is the result of 
   the sum of both floats.
   Examples = 
          (+.) 2. +. 5. = 7.
          (+.) 3.5 + 8. = 11.5
          (+.) 4.3 +. 2.6 = 6.9 *)

(* Specification for (/.):
   Profile = (/.): float -> float -> float
   Semantique = Computes the division between two floats, and returns a single float which is the result
   of the division between both floats.
   Examples = 
            (/.) 4. /. 2. = 2.
            (/.) 7. /. 2. = 3.5
            (/.) 0. /. 3434.3 = 0. *)



(*2.5.2 Fonctions de conversion*)


(*Specification 1:
  Profil = moyenne : Z → Z → R
  Semantique = (moyenne 𝑎 𝑏) est la moyenne de 𝑎 et 𝑏.
  Realisation 1:
  Algorithme = La moyenne est le milieu du segment [a,b] *)
(*Implementation : *)
(*let moyenne (a:int) (b:int) : float =
  (a +. b) /. 2.0;; *)
(*There seems to be an error inside the function, we're adding an ints with floats which
  is prohibited by Ocaml.*)

(*Q.3*)

(*Profile = float_of_int: int -> float *)
float_of_int (3);; (*We obtain 3., which makes sense since we converted the integer 3 to a float.*)
(*float_of_int 3.4;;*) 
(*float_of_int 3.4 *)
(*We get an error because the function should receive an integer as a parameter but we input a float. *)

(*Q.4*)


let moyenne (a:int) (b:int) : float = (float_of_int a +. float_of_int b)/. 2.0;;
(moyenne) 0 0;;
(* Some tests of the function:
   moyenne 5 7 -> 6
   moyenne (-2) 7 -> 2.5
   moyenne (-5) 2 -> 3  
   moyenne  0 6 -> 3
   moyenne 0 0 -> 0 *)

(*Q.5*)

(*Specification int_of_float :
  Profile = int_of_float: float -> int
  Semantique = Takes a float and converts it to an integer.
  Examples:
     int_of_float 5. -> 5
     int_of_float 0. -> 0
     int_of_float (-6.) -> -6 *)
int_of_float (-6.);;

(* 
 EXERCICE 2.6
 Q1.
*)

(* DEFINING FUNCTIONS *)

let max2 (x:int) (y:int): int =
	(abs(x-y)+(x+y))/2
;;

let min2 (x:int) (y:int): int =
	if max2 x y = x then y else x
;;

let max4 (a:int)(b:int) (c:int) (d:int): int =
	max2 (max2 a b) (max2 c d)
;;

let min4 (a:int) (b:int) (c:int) (d:int): int =
	min2 (min2 a b) (min2 c d)
;;

let moyol (a:int) (b:int) (c:int) (d:int): float =
	let max = max4 a b c d and min = min4 a b c d in
		float_of_int((a+b+c+d)-(max+min)) /. 2.;;
;;

(* Q2. *)

(* FONCTION TESTS *)

(* test max2 *)
max2 0 0;;
max2 1 0;;
max2 10 5;;
max2 1 (-1);;
max2 (-1) (-2);;

(* test min2 *)
min2 0 0;;
min2 1 0;;
min2 10 5;;
min2 1 (-1);;
min2 (-1) (-2);;

(* test min4 *)
min4 0 0 0 0;;
min4 1 0 1 0;;
min4 1 1 0 0;;
min4 10 5 2 3;;
min4 1 (-1) 1 (-1);;
min4 (-1) (-2) (-3) (-2);;

(* test max5 *)
max4 0 0 0 0;;
max4 1 0 1 0;;
max4 1 1 0 0;;
max4 10 5 2 3;;
max4 1 (-1) 1 (-1);;
max4 (-1) (-2) (-3) (-2);;

(* test moyol *)
moyol 0 0 0 0;;
moyol 1 0 0 1;;
moyol 20 5 7 48;;
moyol (-1) 0 (-1) 0;;
moyol (-3) (-56) (-14) (-10);; 

(* 
 Q3.
 tracing a test 
*)

#trace moyol;;
#trace max2;;
#trace min2;;
#trace max4;;
#trace min4;;

moyol 20 5 7 48;;

#untrace moyol;;
#untrace max2;;
#untrace min2;;
#untrace max4;;
#untrace min4;;

(* 
Q4.
identing (moyol 20 5 7 48) trace:

moyol <-- 20
moyol --> <fun>
moyol* <-- 5
moyol* --> <fun>
moyol** <-- 7
moyol** --> <fun>
moyol*** <-- 48

	max4 <-- 20
	max4 --> <fun>
	max4* <-- 5
	max4* --> <fun>
	max4** <-- 7
	max4** --> <fun>
	max4*** <-- 48
	
		max2 <-- 7
		max2 --> <fun>
		max2* <-- 48
		max2* --> 48
		
		max2 <-- 20
		max2 --> <fun>
		max2* <-- 5
		max2* --> 20
		
		max2 <-- 20
		max2 --> <fun>
		max2* <-- 48
		max2* --> 48
		
	max4*** --> 48
	
	min4 <-- 20
	min4 --> <fun>
	min4* <-- 5
	min4* --> <fun>
	min4** <-- 7
	min4** --> <fun>
	min4*** <-- 48
	
		min2 <-- 7
		min2 --> <fun>
		min2* <-- 48
			max2 <-- 7
			max2 --> <fun>
			max2* <-- 48
			max2* --> 48
		min2* --> 7
		
		min2 <-- 20
		min2 --> <fun>
		min2* <-- 5
			max2 <-- 20
			max2 --> <fun>
			max2* <-- 5
			max2* --> 20
		min2* --> 5
		
		min2 <-- 5
		min2 --> <fun>
		min2* <-- 7
			max2 <-- 5
			max2 --> <fun>
			max2* <-- 7
			max2* --> 7
		min2* --> 5
		
	min4*** --> 5
moyol*** --> 13.5
*)

