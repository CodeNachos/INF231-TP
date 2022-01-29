(*
inf201_Doktorcik_Abdelkader_MascarenhasTP1.ml : exercices TP1 no1

Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr>            \
Maxence DOKTORCIK <Maxence.Doktorcik@etu.univ-grenoble-alpes.fr>               > Groupe Ocaml_best_camel
Rafael MASCARENHAS <Rafael.Mascarenhas-Dalle-Nery@etu.univ-grenoble-alpes.fr> /
*)


(* 
   EXERCICE 2.1.1
   Q1 Trying different commands
*)

24;;        (* Displays the value and type of the integer value 24*)

3+4;;       (* Does the sum of 3 and 4 and since they are both int it computes an int which is 7 *)  

3+5.3;;     (* This doesn't work because OCaml sees the + first and then the type of the object on the left so an int \
            and expect the next object to be an int but it is a float so the error is Error: This expression has type \
            float but an expression was expected of type int *)

6.1+8.2;;   (* This time it doesn't work because the object are both floats but + only wotks for int objects otherwise \
            it's +. *)

3.2+.6.1;;  (* This is what we said before so it works and returns 9.3 *)

6+.5;;      (* This time 6+.5 is not the addition of 2 floats with a +. but actually it thinks that +.5 is a float \
            (which is then 0.5) but since 6 is an int the 2 objects are different and the + only does int + int. \
            Error: This expression has type int but an expression was expected of type float *)

6.+.5.;;    (* This gives 11. because with a have two float 6. and 5. and we have the function +. so it adds two floats\
            which is the case *)

(* Q2 Trying different commands *)

'f';;     (* Returns the character f *)

'5';;     (* Returns the character 5 *)

'3'+4;;   (* + only works for int but '3' is a character. Error: This expression has type char but an expression was \
          expected of type int *)

'3+4';;   (* 'char' is only used for one single character but here we have multiple characters comprehends something \
          else. Error: Invalid literal 4' *)

'x';;     (* Returns the character x *)

x;;       (* We didn't define x so it is an unbound value that gives an error. Error: Unbound value x *)

(* Q3 Trying different commands *)

true;;            (* Returns a bool which is true *)

false;;           (* Same but with false *)

true && false;;   (* Returns false because of the table of truth as expected *)

true || false;;   (* Returns true as expected because of the table of truth *)

not(false);;      (* Returns the opposite of false so true *)

(* Q4 Trying different commands *)

4 + 3 * 2 ;;          (* Does the multiplication first and then add the integer on the left side of the + and the new \
                      integer on the right so 4+6 and gives 10 *)

4 + 3 / 2 ;;          (* The function / gives the integer part of the divison of the 2 integer so here 3/2 is 1 and \
                      then 4+1 wotks and gives 5(functions will be explained later on) *)

(4 + 3) / 2 ;;        (* Does first 4+3 which works and makes 7 and divide this integer by the other one so 2 but as \
                      we said it gives the integer part so 3 *)

4 + 3 /. 2 ;;         (* It will first check the division but here we have /. so it is dividing 2 floats but 3 isn't \
                      one and the command will break by seeing it's an integer before seeing something else. \
                      Error: This expression has type int but an expression was expected of type float *)
(4. +. 3.) /. 2. ;;   (* All of this works because what's in parentheses adds two floats with +. \ 
                      so float -> float -> float and then divide the float by another float with /. \
                      so float -> float -> float which is then the cases and gives 3.5 *)

10 mod 5 ;;           (* Gives the rest of the division of 10/5 which is 0 *)

10 mod 3 ;;           (* Gives the rest of the division of 10/3 which is 1 *)

10 mod 0 ;;           (* Tries to give the rest of the division of 10/0 which doesn't exist but it doesn't raise an \
                      error and only show Exception: Division_by_zero. *)

(*

Q4/Conclusion

For the function /
(/) : int -> int -> int 
Semantic : The function takes 2 int and returns the integer part of the division of the two int (which is then an int)
Algorithm : Use of (/)
If we do a division by 0 it raises the fact that it cannot do it by saying it's an exception since we can't divide by 0

For the function (/.)
(/.) : float -> float -> float 
Semantic : The function takes 2 float and returns the division of the two float as a float
Algorithm : Use of (/.)
If we do a division by 0. it raises the fact that it cannot do it by saying it's an exception since we can't divide by 0

For the function mod
mod : int -> int -> int 
Semantic : The function takes 2 int and returns the rest of the division of the two int (which is an int)
Algorithm : Use of mod
If we do a mod 0 it raises the fact that it cannot do it by saying it's an exception since we can't divide by 0

*)

(* 
   EXERCICE 2.1.2
   Q.5
*)

2 =3;;            (* Since we're using an '=', a boolean is therefore expected, which is false since 2 is not 3. *)

'e' = 't';;       (* Same can be said here, except the types chosen here are characters rather than integers. *)

false = false;;   (* Essentially, we're comparing false to false, which is true since false is indeed false. *)

4 = false;;       (* This raises an error because we're comparing two different types (bool and int) *)

'3'=3;;           (* This also raises an error since we're comparing two different types (character and int) *)

6.=6.;;           (* Comparing two floats that have the same value, so a 'true' was the expected boolean. *)

8.1=7;;           (* Raises an error because we're comparing a float with an integer. *)

(* Q.6 *)

2<3;;           (* A boolean with value 'true' is expected since 2 is smaller than three. *)

'e'<'t';;       (* A boolean with value 'true' is expected since it respects the comparaison respects alphabetical \
                order. *)

false<true;;    (* With true being 1 and false being 0, the expression is true*)

true<false;;    (* Logically, false is expected. *)

4<false;;       (* An error is expected since we're comparing two different types with each other. *)

'4'<'6';;       (* The characters are converted into ASCII code, so naturally the asnwer is true. *)

2>=3;;          (* False is logically expected. *)

2> =3;;         (* We get a syntax error since there is no defined operator. *)

2<>2;;          (* We get false since 2 is equal to 2. *)

(* Q.7 *)

(2=3)=true;;  (* We got false as an answer. *)

not(2=3);;    (* Logically we got true. *)

(2=3)=false;; (* We get true as an answer. *)

false=(2=3);; (* We get true as an answer. *)

(* From our answers, we're able to deduce that ocaml prioritizes the expression between
parenthesis first, then evaluates the rest. *)

(* Q.8 *)

false=2=3;; (* We get an error, stating that Ocaml was expecting a boolean type.*)

2=3=false;; (* We get true as an answer.*)

(* We deduce therefore that '=' is of left associativity. *)

(* Q.9 *)

2<3<4;;         (* We get an error stating that Ocaml expected a boolean expression *)

2 = 3-1 = 4-2;; (* We get the same error. *)

(* We can conclude that there seems to be some sort of order that ocaml respects, it evaluates
an expression before another and doesn't run the line all at once. *)

(* Q.10 *)

2<3 && 3<4;;              (* We get true as an answer. *)

not (4<2) || false;;      (* We get true as an anwser since true or false is true *)

not true && false;;       (* We get false as an answer. *)

true || true && false;;   (* We get true as an answer. *)

(* We deduce that not (no) has priority over && (and) and || (or). We also deduce that && and || are 
right associative. *)

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
- : int = 8              As expected it prints the type and value of a *)

a + 5 ;;                (* It will display the value and type of the expression 8 + 5
- : int = 13            It prints 13 as expected *)

a +. 9.1 ;;             (* This operation will raise an error as it is a float operation \
                        and a is of type int.
Error:This expression has type int but an expression was expected of type float *)

(*
   EXERCICE 2.1.5
   Q14
*)

let a:int = 8;;                     (* We set the value of a*)

if a < 10 then true else a;;        (* The rule with an if is that the type that the function will give if the \
                                    condition is true should be the same as the one it will give if it doesn't \
                                    complete it but here in the if it returns true so the other one in then should be \
                                    also a bool but a is an int so it doesn't work *)

if a < 10 then a else false;;       (* As we said before the output should be of the same of the same type for every \
                                    possibilities so this one doesn't work because a is an int and so we should also \
                                    have an int after the then which isn't the case *)

if a < 10 then a;;                  (* Another rule in the if is that an else is mandatory because the condition \
                                    cannot always be met (even if here it is) and so OCaml will simply not run it if \
                                    there is no then in the function or the then as to be of type unit in which case \
                                    it will always give unit *)

if a < 10 then true else false;;    (*This meets all the rules*)


(*
   EXERCICE 2.2.1
   Q.1
*)

let max2 (a: int) (b: int): int =( (a+b) + abs(a-b) ) / 2;;

(* We observe val max2 : int-> int -> int = <fun> *)

(* Q.2 *)

abs;;

(* We observe - : int -> int = <fun> *)

(*
Specification for abs:
  - Profile : abs: int -> int
  - Semantique : The function abs takes a single integer n as parameter. If n>0, then the function return n,
    if n<0 then the function return -n and if n=0, then the function returns 0.
*)

(* Tests for function abs: *)
abs(6);;    (* (a) Test with an integer n=6, returns n since n>0. *)

abs(-3);;   (* (b) Test with an integer n=-3, returns -n (so 3) since n<0 *)

abs(0);;    (* (c)Test with an integer n=0, returns 0 since n=0. *)

(*
   EXERCICE 2.2.2
   Q.3
*)

let max2 (a: int) (b: int): int =( (a+b) + abs(a-b) ) / 2;;

(max2) 5 4;;          (* Testing with two integers 5 and 4.*)

(max2) -7 -9;;        (* Interesting error, forgot parenthesis for the minus, ocaml processes this as a substraction \
                      rather than a single integer.*)

(max2) 0 0;;          (* Works with 0 as well. *)

(max2) (-7) (-9);;    (* Testing with negative integers. *)

(max2) (4.) (6);;     (* Testing max with a float, but doesn't work since ocaml expects integer type. *)

(max2) ('4') ('8');;  (* Doesn't work for same reason stated above, ocaml only expects integer types. *)

(* Q.4 *)

assert ((max2 4 7) = 7) ;;      (* We get a unit type. *)

assert ((max2 4 7) = 5) ;;      (* We get an Exception: assert_failure *)

assert ((max2 '4' 7) = 7) ;;    (* We get an Error, due to different types being implemented. *)

(* Therefore, when the result is correct, ocaml sends a unit type. *)

(*
   EXERCICE 2.2.3
    Q.5
*)

let max3_vl (a:int)(b:int)(c:int):int = if a>=b && a>=c then a else if b>=a && b>=c then b else (*c>=a && c>=b*) c;;

(max3_vl) 14 16 10;;  (* Test with three different values. *)
(max3_vl) 15 10 10;;  (* Test with two different values. *)
(max3_vl) 10 10 10;;  (* Test with same numbers *)

let x=10 in let x = 3 and y = x+4 in x+y;;

let x=10 in
  (let x=3 and y=x+4 in x+y) + x ;;

let x=10 in 
  let x=3 and y=x+4 in 
    x+y + x ;;

let x = 5 and y = x+5;;

(*
   EXERCISE 2.3
   Q1
*)
let x=3 and y=4 in x+y ;;           (* This function will just create local variables in x+y that are x=3 and y=4 but \
                                    then this function won't have any name so we can't call it but it gives 7. \
                                    Otherwise we should write let name = let x=3 and y=4 in x+y;; in which case it \
                                    will have the name "name" *)

let x=3 and y=x+4 in x+y ;;         (* x is not defined because it runs parallelly and so x is not yet defined and it \
                                    just raises an error since y uses x. Error: Unbound value x *)

let x=10 in                         (* This time the function will define x=10 in the function y=x+4 because it's *)
  let x=3 and y=x+4 in              (* nested inside so yis now 14, then we have the other function where x is now 3 *)
    x+y                             (* because this x is nested inside the function x+y so 3+14=17* but be aware *)
;;                                  (* because all the variable are local and the function as no name *)


let x=10 in                         (* Since x=10 is in the whole expression it is nested inside y=x+4 and x at the *)
  (let x=3 and y=x+4 in x+y) + x    (* end of the line and the x=3 is only used in the parantheses as in x+y so if we *)
;;                                  (* do the whole operation we have 3+(10+4)+10 *)

let x=10 in                         (* Since x=10 in the whole expression then it is being used and so it sued in *)
  let x=3 and y=x+4 in x+y + x      (* y=x+4 and so y is 14 but then the x=3 belongs to the x+y+x because we have an *)
;;                                  (* in after so we have 3+14+3 which is 20 *)

x ;;                                (* We didn't specify x in the end because all the variables were local so it's an \
                                    unbound value *)

(* We can conclude that it depends on the way the expression is nested, the parentheses and the positions of the \
keyword 'in' determine the values that a certain variable will take *)