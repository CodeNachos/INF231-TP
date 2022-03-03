(* -----------------------------------------------------------------------
   inf201-ABDELKADER-MASCARENHAS-DOKTORCIK-TP3.ml : cr exercices TP no5
   Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr>  \ 
   Rafael MASCARENHAS <Rafael.Mascarenhas@etu.univ-grenoble-alpes.fr>   > Groupe Ocaml_best_camel
   Maxence DOKTORCIK <Maxence.Doktorcik@etu.univ-grenoble-alpes.fr>    / 
----------------------------------------------------------------------- *)


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
