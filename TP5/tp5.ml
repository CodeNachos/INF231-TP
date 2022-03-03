(* -----------------------------------------------------------------------
   inf201-ABDELKADER-MASCARENHAS-DOKTORCIK-TP3.ml : cr exercices TP no5
   Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr>  \ 
   Rafael MASCARENHAS <Rafael.Mascarenhas@etu.univ-grenoble-alpes.fr>   > Groupe Ocaml_best_camel
   Maxence DOKTORCIK <Maxence.Doktorcik@etu.univ-grenoble-alpes.fr>    / 
----------------------------------------------------------------------- *)
(*3.1 Factorielle*)

(*Q.1*)

let rec factorielle (p:int):int = match p with
  |0 -> 1
  |_ -> p * factorielle (p-1);;

(*Q.2*)

(*Test with 5*)
(factorielle) 5;;

(*Test with 4*)
(factorielle) 4;;

(*Tracing of the function:
  factorielle <-- 5
   factorielle <-- 4
      factorielle <-- 3
         factorielle <-- 2
            factorielle <-- 1
               factorielle <-- 0
               factorielle --> 1
            factorielle --> 1
         factorielle --> 2
      factorielle --> 6
   factorielle --> 24
  factorielle --> 120
  - : int = 120*)

(*Q.3*)

(*(factorielle) (-1);;*)

(*We get an error: Stack overflow during evaluation (looping recursion?), which 
  means that the recursion never ends. This makes sense, since we keep removing -1 from the initial
  parameter and the recursion ends once eventually the difference reaches 0. If we input
  a negative number in the first place, the parameter never reaches 0. The recursion 
  goes on forever.*)

(*Tracing of (factorielle) (-1)*)
(*        
# (factorielle) (-1);;
factorielle <-- -1
factorielle <-- -2
factorielle <-- -3
factorielle <-- -4
factorielle <-- -5
factorielle <-- -6
factorielle <-- -7
factorielle <-- -8
factorielle <-- -9
factorielle <-- -10
factorielle <-- -11
factorielle <-- -12
factorielle <-- -13
factorielle <-- -14
factorielle <-- -15
factorielle <-- -16
factorielle <-- -17
factorielle <-- -18
factorielle <-- -19
factorielle <-- -20
factorielle <-- -21
factorielle <-- -22
factorielle <-- -23
factorielle <-- -24
factorielle <-- -25
factorielle <-- -26
factorielle <-- -27
factorielle <-- -28
factorielle <-- -29
factorielle <-- -30
Stack overflow during evaluation (looping recursion?).#
As we can see, the function keeps going on forever. *)

(*Q.4*)

let factorielle2 (p:int):int = match p with
  |0 -> 1
  |_ -> p * factorielle2 (p-1);;

(*When we forget the 'rec', we get the following error:
  Error: Unbound value factorielle.
  We also get the same error when defining the function itself. (Additionaly, Ocaml suggests us to 
  add 'rec' : 
  Hint: Did you mean factorielle?
  Hint: If this is a recursive definition,
  you should add the 'rec' keyword on line 1). 
  Since the function is not recursive, Ocaml interprets the 'factorielle' that is in 
  the body of the function as an undefined variable.*)

(*Q.5*)

let rec factorielle3 (p:int):int = match p with
  |p -> p * factorielle (p-1)
  |0 -> 1;; 

(*When defining the function, we get the following message:
  Warning 11 [redundant-case]: this match case is unused.*)

(*Test of factorille3:*)
(*(factorielle3 ) 5;; *)

(*When we test the function (with whatever value), we get the following error:
  Stack overflow during evaluation (looping recursion?).*)

(*Which makes sense. The first error we get when defining the function tells us that
  the second case in pattern matching is "not used", which is true since we'll be always
  matching the parameter p with itself, and p cannot be different than itself at any given moment,
  so the other case is useless. When we give it a parameter, the code doesn't work
  since the recursion doesn't end (since we're continuously matching p with itself), that's
  why it raises an error.*)

(*Q.6*)

(*We suppose that all constraints are respected*)
let rec fact3 (p:int):int = if p = 0 then 1 else p * fact3 (p-1);;
(fact3) 6;;



-----
(*
   Exercice 3.3.1
   Q1.
*)

(* Definition of type to represent the set N of natural numbers
   /!\ Note that nat_number n is any integer bigger or euqal to 0 only
*)
type nat_number = int ;;

(* Q2. *)

(*
  REALISATION quotient

  Algorithme: We proceed by successive subtractions ; its necessary to express
               (quotient a b) in function of (quotient (a-b) b)
  
  Equations recursives:
              (1) (quotient a b) =  0                         if a < b
              (2) (quotient a b) =  1 + (quotient (a-b) b)    else
  
  Realisation:
  /!\ Note that b cannot be 0
*)
let rec quotient (a:nat_number) (b:nat_number) : nat_number =
  if a < b then 0 else 1+ (quotient (a-b) b)
;;

(* Q3. (a) *)

assert((quotient 0 1)=0);;
(* Output: - : unit = () *)
assert((quotient 1 2)=0);;
(* Output: - : unit = () *)
assert((quotient 1 1)=1);;
(* Output: - : unit = () *)
assert((quotient 2 1)=2);;
(* Output: - : unit = () *)
assert((quotient 4 2)=2);;
(* Output: - : unit = () *)
assert((quotient 5 2)=2);;
(* Output: - : unit = () *)

(* Q3. (b) *)

(* When the constraints over b is not respected the condition for the recursion
to end is not respected as a-b = a. The interpreted rises an exception:

quotient 1 0 ;;
Output: Exception: Assert_failure (...).
*)

(* Q3. (c) *)

#trace quotient;;
quotient 17 5 ;;
#untrace quotient;;

(* Trace output: 

quotient <-- 17
quotient --> <fun>
quotient* <-- 5
  quotient <-- 12
  quotient --> <fun>
  quotient* <-- 5
    quotient <-- 7
    quotient --> <fun>
    quotient* <-- 5
      quotient <-- 2
      quotient --> <fun>
      quotient* <-- 5
      quotient* --> 0
    quotient* --> 1
  quotient* --> 2
quotient* --> 3
- : nat_number = 3    (Final result)
*)

(* Q4. *)

(*
  REALISATION reste

  Algorithme: We proceed by successive subtractions ; its necessary to express
               (reste a b) in function of (reste (a-b) b)
  
  Equations recursives:
              (1) (reste a b) =  a                            if a < b
              (2) (reste a b) =  (reste (a-b) b)              else
  
  Realisation:
  /!\ Note that b cannot be 0
*)
let rec reste (a:nat_number) (b:nat_number) : nat_number =
  if a<b then a else reste (a-b) b
;;

assert((reste 0 1)=0);;
(* Output: - : unit = () *)
assert((reste 1 1)=0);;
(* Output: - : unit = () *)
assert((reste 2 1)=0);;
(* Output: - : unit = () *)
assert((reste 4 2)=0);;
(* Output: - : unit = () *)
assert((reste 1 2)=1);;
(* Output: - : unit = () *)
assert((reste 5 2)=1);;
(* Output: - : unit = () *)

(*
   Exercice 3.3.2
   Q5.
*)

(*
  SPECIFICATION qr1

  Profile:    qr1 : N -> N -> N*N

  Semantics:  Let (q,r) = (qr1 a b)
              Then the 2-uplet (q,r) contains the quotient and the rest of the 
              division of a by b. Where q is the quotient and r is the rest

  Exemple:    (qr1 17 5) = (3,2)
*)

(* Q6. *)

let qr1 (a:nat_number) (b:nat_number) : nat_number*nat_number = 
  ((quotient a b), (reste a b))
;;

(* Q7. *)

#trace qr1;;
#trace quotient;;
#trace reste;;
qr1 17 5;;
#untrace qr1;;
#untrace quotient;;
#untrace reste;;

(* Trace output:
qr1 <-- 17
qr1 --> <fun>
qr1* <-- 5
  reste <-- 17
  reste --> <fun>
  reste* <-- 5
    reste <-- 12
    reste --> <fun>
    reste* <-- 5
      reste <-- 7
      reste --> <fun>
      reste* <-- 5
        reste <-- 2
        reste --> <fun>
        reste* <-- 5
        reste* --> 2
    reste* --> 2
  reste* --> 2
reste* --> 2
  quotient <-- 17
  quotient --> <fun>
  quotient* <-- 5
    quotient <-- 12
    quotient --> <fun>
    quotient* <-- 5
      quotient <-- 7
      quotient --> <fun>
      quotient* <-- 5
        quotient <-- 2
        quotient --> <fun>
        quotient* <-- 5
        quotient* --> 0
      quotient* --> 1
    quotient* --> 2
  quotient* --> 3
qr1* --> (3, 2)
- : nat_number * nat_number = (3, 2)

When calling this function, even if it is not recursive, it uses recursive 
functions that operate by the same principle. We can see by the function tracing
that calling both quotient and reste repeats the same process two times. This 
process could be optimised by making the fucntion qr recursive, following the 
same operating principle of quotient and reste.
*)

(* Q8. *)

(*
  REALISATION qr2

  Algorithme: We proceed by successive subtractions ; its necessary to express
               (reste a b) in function of (reste (a-b) b)
  
  Equations recursives:
              (1) (qr2 a b) =  (0,a)                              if a < b
              (2) (qr2 a b) =  let (q,r)=(qr2 (a-b) b) in (q+1,r) else
  
  Realisation:
  /!\ Note that b cannot be 0
*)
let rec qr2 (a:nat_number) (b:nat_number) : nat_number*nat_number =
  if a < b then (0,a) else let (q,r)=(qr2 (a-b) b) in (q+1,r)
;;

(* Q9. *)

#trace qr2;;
qr2 17 5 ;;
#untrace qr2;;

(* Trace output:
qr2 <-- 17
qr2 --> <fun>
qr2* <-- 5
  qr2 <-- 12
  qr2 --> <fun>
  qr2* <-- 5
    qr2 <-- 7
    qr2 --> <fun>
    qr2* <-- 5
      qr2 <-- 2
      qr2 --> <fun>
      qr2* <-- 5
      qr2* --> (0, 2)
    qr2* --> (1, 2)
  qr2* --> (2, 2)
qr2* --> (3, 2)
- : nat_number * nat_number = (3, 2)      (final result)

This time we see that theres is no redundant calculations in the function 
tracing. The process is computed only once by function call
*)
