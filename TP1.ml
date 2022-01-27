(*
inf201_Doktorcik_Abdelkader_MascarenhasTP1.ml : exercices TP1 no1

Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr> \
Maxence DOKTORCIK <Maxence.Doktorcik@etu.univ-grenoble-alpes.fr>    > Groupe Ocaml_best_camel
Rafael MASCARENHAS <Rafael.Mascarenhas@etu.univ-grenoble-alpes.fr> /
*)



___________________________________

(*exercice 2.1.2*)
(*Q.5*)
2 =3;;  (*Since we're using an '=', a boolean is therefore expected, which is false since 2 is not 3.*)
'e' = 't';; (*Same can be said here, except the types chosen here are characters rather than integers.*)
false = false;; (*Essentially, we're comparing false to false, which is true since false is indeed false.*)
4 = false;; (*This raises an error because we're comparing two different types (bool and int)*)
'3'=3;; (*This also raises an error since we're comparing two different types (character and int)*)
6.=6.;; (*Comparing two floats that have the same value, so a 'true' was the expected boolean.*)
8.1=7;; (*Raises an error because we're comparing a float with an integer.*)

(*Q.6*)
2<3;; (*A boolean with value 'true' is expected since 2 is smaller than three.*)
'e'<'t';; (*A boolean with value 'true' is expected since it respects the comparaison respects alphabetical order.*)
false<true;; (*With true being 1 and false being 0, the expression is true*)
true<false;; (*Logically, false is expected.*)
4<false;; (*An error is expected since we're comparing two different types with each other.*)
'4'<'6';; (*The characters are converted into ASCII code, so naturally the asnwer is true.*)
2>=3;; (*False is logically expected.*)
2> =3;; (*We get a syntax error since there is no defined operator.*)
2<>2;; (*We get false since 2 is equal to 2.*)

(*Q.7*)

(2=3)=true;; (*We got false as an answer.*)
not(2=3);;(*Logically we got true.*)
(2=3)=false;; (*We get true as an answer.*)
false=(2=3);; (*We get true as an answer.*)
(*From our answers, we're able to deduce that ocaml prioritizes the expression between
  parenthesis first, theh evaluates the rest.*)

(*Q.8*)

false=2=3;; (*We get an error, stating that Ocaml was expecting a boolean type.*)
2=3=false;; (*We get true as an answer.*)
(*We deduce therefore that '=' is of left associativity.*)

(*Q.9*)

2<3<4;; (*We get an error stating that Ocaml expected a boolean expression*)
2 = 3-1 = 4-2;; (*We get the same error.*)
(*We can conclude that there seems to be some sort of order that ocaml respects, it evaluates
  an expression before another and doesn't run the line all at once.*)

(*Q.10*)
2<3 && 3<4;; (*We get true as an answer.*)
not (4<2) || false;; (*We get true as an anwser since true or false is true*)
not true && false;; (*We get false as an answer.*)
true || true && false;; (*We get true as an answer.*)
(*We deduce that not (no) has priority over && (and) and || (or). We also deduce that && and || are 
  right associative.*)

(* 
   EXERCICE 2.1.3 
   Q11.
*)

assert ((5=3) = false);;        (* This expression will output nothing (unit) \
 - : unit = ()                     So the expression is correct *)

assert ((5=5) = true);;         (* This expression will output nothing (unit) \
 - : unit  = ()                    So the expression is correct *)

assert ((5=3) = true);;         (* This expression will raise an exception as 5 <> 3 \
Exception: Assert_failure ("//toplevel//", 1, 0). *)

(* 
   One can conclude that the expression "assert " will raise an error when the boolean \
value returned by a given expression is false.
*)

(* 
   Q 12. As expected for the third expression, an assertion is raised
*)

(*
   EXERCICE 2.1.4
   Q31.
*)

let a:int = 8 ;;        (* Declaring int a *)

a ;;                    (* This will output the type and value of the variable a 
 - : int = 8               As expected it prints the type and value of a*)

a + 5 ;;                (* It will display the value and type of the expression 8 + 5
 - : int = 13              It prints 13 as expected *)

a +. 9.1 ;;             (* This operation will raise an error as it is a float operation \
                           and a is of type int 
Error:This expression has type int but an expression was expected of type float *)


______________________________________



(*2.2.1*)
(*Q.1*)
let max2 (a: int) (b: int): int =( (a+b) + abs(a-b) ) / 2;;

(*We observe val max2 : int-> int -> int = <fun>*)

(*Q.2*)
abs;;

(*We observe - : int -> int = <fun>*)

(*Specification for abs:
  - Profile : abs: int -> int
  - Semantique : The function abs takes a single integer n as parameter. If n>0, then the function return n,
    if n<0 then the function return -n and if n=0, then the function returns 0.*)
(*Tests for function abs:*)
(*a*) abs(6);; (*Test with an integer n=6, returns n since n>0.*)
(*b*) abs(-3);; (*Test with an integer n=-3, returns -n (so 3) since n<0*)
(*c*) abs(0);; (*Test with an integer n=0, returns 0 since n=0.*)

(*2.2.2*)
(*Q.3*)
let max2 (a: int) (b: int): int =( (a+b) + abs(a-b) ) / 2;;
(max2) 5 4;; (*Testing with two integers 5 and 4.*)
(max2) -7 -9;; (*Interesting error, forgot parenthesis for the minus, ocaml processes this as a 
                 substraction rather than a single integer.*)
(max2) 0 0;; (*Works with 0 as well.*)
(max2) (-7) (-9);; (*Testing with negative integers.*)
(max2) (4.) (6);; (*Testing max with a float, but doesn't work since ocaml expects integer type.*)
(max2) ('4') ('8');; (*Doesn't work for same reason stated above, ocaml only expects integer types.*)

(*Q.4*)

assert ((max2 4 7) = 7) ;; (*We get a unit type.*)
assert ((max2 4 7) = 5) ;; (*We get an Exception: assert_failure*)
assert ((max2 '4' 7) = 7) ;; (*We get an Error, due to different types being implemented.*)
(*Therefore, when the result is correct, ocaml sends a unit type.*)

(*2.2.3*)
(*Q.5*)
let max3_vl (a:int)(b:int)(c:int):int = if a>=b && a>=c then a else if b>=a && b>=c then b else (*c>=a && c>=b*) c;;
(max3_vl) 14 16 10;; (*Test with three different values.*)
(max3_vl) 15 10 10;; (*Test with two different values.*)
(max3_vl) 10 10 10;; (*Test with same numbers*)
let x=10 in let x = 3 and y = x+4 in x+y;;
let x=10 in
(let x=3 and y=x+4 in x+y) + x ;;
let x=10 in let x=3 and y=x+4 in x+y + x ;;
let x = 5 and y = x+5;;

