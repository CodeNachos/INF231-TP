(* 
   Exercice 2.8
   Q1.
*)

(*
true;; false;; vrai;;             When evaluated, the expression will cause the
                                  interpreter to throw an error as 'vrai' is not 
                                  deffined:
Output:
6 | true;; false;; vrai;;
                   ^^^^
Error: Unbound value vrai
*)

(*
true and true ;; true && true ;;        When evaluating this expression, the 
                                        interpreter will throw a syntax error as
                                        'and' is not a valid operator:
Output: 
15 | true and true ;; true && true ;;
          ^^^
Error: Syntax error
*)

(*
2 < 3 ;; 2 >= 3 ;; 2 > = 3 ;; 2 <> 2 ;;   When evaluating this expression, the 
                                          interpreter will throw a syntax error
                                          as it will consider '> =' as two 
                                          different operators, thus evaluating
                                          '2 > (= 3)' 
Output:                                   
25 | 2 < 3 ;; 2 >= 3 ;; 2 > = 3 ;; 2 <> 2 ;;
                            ^
Error: Syntax error
*)

(*
2<3<4 ;; 2<3 && 3<4 ;; 2=3=true ;; 2=(3=true) ;;  When evaluated, 
                                                  the expression will cause the
                                                interpreter to throw an error 
                                             as in the expression '2<3<4' after 
                                           comparing 2 and 3 it will compare the 
                                        result that is a boolean with 4, that is 
                                        a int type. Similarly an error will 
                                        occur for the expression '2=(3=true)' :
Output:
# 2<3<4 ;; 2<3 && 3<4 ;; 2=3=true ;; 2=(3=true) ;;
Error: This expression has type int but an expression was expected of type
         bool
*)

(*
not (4 <= 2) ;; not 4 <= 2 ;;   When evaluating this expression, the interpreter
                                will throw an error as in the expression ' not 
                                4 <= 2' not expects a boolean value while the
                                input is 4 (an int type) because it evaluates 
                                the expression as '(not 4) <= 2' :
Output:                           
53 | not (4 <= 2) ;; not 4 <= 2 ;;
                         ^
Error: This expression has type int but an expression was expected of type
         bool
*)

(* Q2. *)

(* FIRST EXPRESSION *)
not true && false;;         
(* output: - : bool = false *)
(not true) && false;;
(* output: - : bool = false *)
not (true && false);;
(* output: - : bool = true *)

(* SECOND EXPRESSION *)
true || true && false;;
(* output: - : bool = true *)
(true || true) && false;;
(* output: - : bool = false *)
true || (true && false) ;;
(* output: - : bool = true *)

(*
  We can deduce from the first expression that the 'not' operator is prioritary 
  over the and operator '&&' as the original expression and the second computed 
  a 'false' output. We could also verify the priority of 'not' over other 
  operators in the last expression of the last exercice.
  From the second expression it is possible to deduce that the expression and 
  '&&' is prioritary over or '||' as the original expression and the last one 
  tested yielded the same results.
*)

(* Q3. *)

(* 10 mod 0 ;;    This command will raise an exception due to division by zero*) 
10 mod 5 ;;
let essaiEt1 (a:int) (b:int): bool =
(b <> 0) && (a mod b = 0)
;;
essaiEt1 10 5 ;; essaiEt1 10 3 ;; essaiEt1 10 0 ;;
let monEt (x:bool) (y:bool): bool =
x && y
;;
let essaiEt2 (a:int) (b:int): bool =
monEt (b <> 0) (a mod b = 0)
;;
(* essaiEt2 10 0 ;; Will raise an exception due to division by zero*)

(*
 Both functions 'essaiEt1' and 'EssaiEt2' calculates a logical and  in different
 ways. While the former compares directly the outpur of two boolean comparisons,
 the later uses a intermediate function 'monEt' to compute the logical and 
 between to values.
 From the test of both funtions one can deduce that the second therm of an 'and'
 (&&) operation is not always calculated, only when necessary. With the first 
 set of tests using 'essaiEt1' all expressions are interpreted without any 
 exception raised, meaning that the expression 'a mod b = 0' was not evalueated.
  Whowever when using 'essaiEt2' it is needed to calculate the value of the 
  boolean expression 'a mod b = 0' because the function 'monEt' when called 
  require the output of the expression.  
*)

(*
   2.8.1
   Q4.
*)

(10,20,30) ;; ((10,20),30) ;; 20,30.0 ;;
(*
4,3 /. 2,0 ;; 4,3. /. 2,0 ;; 4,3. /. 2.,0 ;;    This sequence of n-uplets 
                                                declarations will raise an error
                                                during its interpretation. The 
                                                interpreter tries to execute the
                                                following whith type unmatching:
                              4,(3 /. 2),0 ;; 4,(3. /. 2),0 ;; 4,(3. /. 2.),0 ;; 
Output:
Error: This expression has type int but an expression was expected of type
         float
  Hint: Did you mean `3.'?

(4,0) /. (2,0) ;;           In this expression the interpreter expects two
                              float values, however to 2-uplets are given.
Output:
142 |   (4,0) /. (2,0) ;;
        ^^^^^
Error: This expression has type 'a * 'b
       but an expression was expected of type float

*)

(*
   2.8.2
   Q5.
*)

type intervalle = (* { (𝑏𝑖, 𝑏𝑠) ∈ Z2 𝑡𝑒𝑙𝑠 𝑞𝑢𝑒 𝑏𝑖 ≤ 𝑏𝑠 } *)
  int * int ;;

(* Q6. *)

let inter_1 : intervalle = (-30, 50)
and x1_1 : int = -40
and x2_1 : int = -30
and x3_1 : int = 30
and x4_1 : int = 50
and x5_1 : int = 51
and x6_1 : int = 60 ;;

let inter_2 : intervalle = (0, 0)
and x1_2 : int = -1
and x2_2 : int = 0
and x3_2 : int = 1 ;;

let inter_3 : intervalle = (1, 5)
and x1_3 : int =  0
and x2_3 : int = 3
and x3_3 : int = 10 ;;

(* Q7. *)

(* SPECIFICATION 

    Profile     precede : int -> intervalle -> bool
    Semantics   (precede x i) if and only if x is strictly less than the 
                integers of i
*)
let precede (x : int) (i :intervalle) : bool =
  let (min, _)=i in
    x < min
;;

(* Indeed the definition of the type intervalle simplifies the definition and
description of the function *)

(* Q8. *)

(*
precede 3 4 5 ;;          This call of precede raises an error as the arguments 
                          are not defined as expected 
Output:
257 | precede 3 4 5 ;;    
      ^^^^^^^
Error: This function has type int -> intervalle -> bool
       It is applied to too many arguments; maybe you forgot a `;'.


precede (3,4,5) ;;        Similarly to the preceding call the arguments are not 
                          well defined
Output:
266 | precede (3,4,5) ;; 
              ^^^^^^^
Error: This expression has type 'a * 'b * 'c
       but an expression was expected of type int
*)

precede 3 (4,5) ;;
(* Output: - : bool = true *)


(* Definition des fonctions 'dans'  and 'suit' 
   (en utilisant aussi le type intervalle) *)

(* SPECIFICATION 

    Profile     dans : int -> intervalle -> bool
    Semantics   (dans x i) if and only if x is included in i
*)
let dans (x : int) (i :intervalle) : bool =
  let (min, max)=i in
    x >= min && x <= max
;;

(* SPECIFICATION 

    Profile     suit : int -> intervalle -> bool
    Semantics   (suit x i) if and only if x is strictly bigger than the integers
                of i
*)
let suit (x : int) (i :intervalle) : bool =
  let (_, max)=i in
    x > max
;;

(* Tests for inter_1 : *)

assert (precede x1_1 inter_1) ;;
assert ((precede x2_1 inter_1) = false) ;;

assert ((dans x1_1 inter_1) = false) ;;
assert (dans x2_1 inter_1) ;;
assert (dans x3_1 inter_1) ;;
assert (dans x4_1 inter_1) ;;
assert ((dans x5_1 inter_1) = false) ;;
assert ((dans x6_1 inter_1) = false) ;;

assert ((suit x4_1 inter_1) = false) ;;
assert (suit x5_1 inter_1) ;;
assert (suit x6_1 inter_1) ;;

(* Tests for inter_2 : *)

assert (precede x1_2 inter_2) ;;
assert ((precede x2_2 inter_2) = false) ;;
assert ((precede x3_2 inter_2) = false) ;;

assert ((dans x1_2 inter_2) = false) ;;
assert (dans x2_2 inter_2) ;;
assert ((dans x3_2 inter_2) = false) ;;

assert ((suit x1_2 inter_2) = false) ;;
assert ((suit x2_2 inter_2) = false) ;;
assert (suit x3_2 inter_2) ;;

(* Tests for inter_3 : *)

assert (precede x1_3 inter_3) ;;
assert ((precede x2_3 inter_3) = false) ;;
assert ((precede x3_3 inter_3) = false) ;;

assert ((dans x1_3 inter_3) = false) ;;
assert (dans x2_3 inter_3) ;;
assert ((dans x3_3 inter_3) = false) ;;

assert ((suit x1_3 inter_3) = false) ;;
assert ((suit x2_3 inter_3) = false) ;;
assert (suit x3_3 inter_3) ;;

(* 
   2.8.3 
   Q9. - Q10.
*)

let cst_I1 : intervalle = (1,5)
and cst_I2 : intervalle = (6, 10)
and cst_I3 : intervalle = (2, 3)
and cst_I4 : intervalle = (0, 1)
and cst_I5 : intervalle = (5, 10)
and cst_I6 : intervalle = (4, 7)
and cst_I7 : intervalle = (1, 2) ;;

(* Declaration of functions 'coteAcote' and 'chebauche' *)

(* SPECIFICATION
   Profile      coteAcote : itervalle -> intervalle -> bool
   Semantics    coteAcote(i1, i2) is true if and only if i1 and i2 are not 
                continguous without intersecting
*)
let coteAcote (i1 : intervalle) (i2 : intervalle) : bool =
  let (min1, max1)=i1 and (min2, max2)=i2 in
    ((max1 + 1) = min2) || ((max2 + 1) = min1)
;;

(* SPECIFICATION 
   Profile      intervalle -> intervalle -> bool
   Semantics    chevauche(i1, i2) is true if and only if i1 and i2 doesnt have 
                common borders, one is not included in the other, but their 
                intersection is not empty
*)
let chevauche (i1 : intervalle) (i2 : intervalle) : bool =
  let (min1, max1)=i1 and (min2, max2)=i2 in
    (* verifies that theres no common border *)
    ((min1 <> min2) && (min1 <> max2) && (max1 <> min2) && (max1 <> max2)) 
    && (* verifies that the intersection is not empty *)
    (((min1 < max2) && (max1 > max2)) || ((max1 > min2) && (min1 < min2)))
;;

(* Tests function coteAcote*)
assert(coteAcote cst_I1 cst_I2);;
assert(coteAcote cst_I3 cst_I4) ;;
assert((coteAcote cst_I1 cst_I5) = false) ;;
assert((coteAcote cst_I1 cst_I6) = false) ;;
assert((coteAcote cst_I1 cst_I1) = false) ;;

(* Tests function chevauche *)
assert(chevauche cst_I1 cst_I6) ;;
assert(chevauche cst_I2 cst_I6) ;; 
assert((chevauche cst_I1 cst_I5) = false) ;;
assert((chevauche cst_I5 cst_I1) = false) ;;
assert((chevauche cst_I1 cst_I7) = false) ;; 
assert((chevauche cst_I2 cst_I5) = false) ;; 
assert((chevauche cst_I5 cst_I2) = false) ;; 
assert((chevauche cst_I1 cst_I1) = false) ;;




(*Exercice 2.9*)
(*Q1/*)
(*Creating the function div as it is defined with the profil and semantic already noted in the poly*)
let div (n:int) (d:int)=
  (n/d,n mod d);; 
(*The function takes 2 integers and returns the couple of the quotient and the rest that we can directly 
write like a couple with the necessary functions we will just have to name each element to use them with let (q,r)=f(...) in ...*)

(*Q2/*)
(*Creating the function div as it is defined with the profil and semantic already noted in the poly and using div*)
(*/!\ Use condition: n as to be between 0 and 9999*)
let sc (n:int):int=
  let (diz,u)=div n 10 in 
    let (cent,d)=div diz 10 in
      let (mil,c)=div cent 10 in
        let m=mil mod 10 in
          m+c+d+u;;
(*This function uses the previous number that we calculate everytime to calculate the next one.
It first does the euclidian division and the rest for the number of unit and then
it calculates the euclidian division of the number of 10's by using the previous division 
and it does the same for 100's. For the number of 1000's it does the same except that
it doesn't calculate the divison because we would have no need for after and we only need the modulo.
Then all of the number of thousands, hundreds,etc... are summed up which gives us the sum of the number
*)
(*Fondamental tests*)
sc 0;; (*Returns 0 as expected and it is the lowest we can have because of our condition*)
sc 9999;; (*Returns 36 as expected and it is the biggest number we can have because of our condition*)
sc 6234;; (*Returns 15 as expected and it is just a random test number*)
(*Q3/*)
#trace div;;
#trace sc;;
sc 2345;;
(*Here is the indent version of the trace
sc <-- 2345
div <-- 2345
div --> <fun>
div* <-- 10
div* --> (234, 5)
    div <-- 234
    div --> <fun>
    div* <-- 10
    div* --> (23, 4)
      div <-- 23
      div --> <fun>
      div* <-- 10
      div* --> (2, 3)
sc --> 14
We can see that it correspond to what we said earlier*)
#untrace div;;
#untrace sc;;
