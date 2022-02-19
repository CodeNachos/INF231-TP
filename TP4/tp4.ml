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

(*Exercice 2.11 *)

(*2.11 Type Durée et opérations associées*)

(*2.11.1 Définition du type duree et des operations associées*)

(*Q.1*)

type jour = int (* {0,...,31} *);;
type heure = int (* {0,...,23} *);;
type de0a59 = int (* {0,...,59} *);;
type minute = int (* {0,...,59} *);;
type seconde = int (* {0,...,59} *);;
type duree = jour * heure * minute * seconde;;

(*Q.1 bis*)

let div (n:int) (m:int) :int*int = (n/m, n mod m);; 
(div) 5 2;;

let sec_en_duree (s:seconde):duree = let (q,r) = div s 60 in match (q,r) with
  |(q,r) when q<60 -> (0,0,q,r)
  |(q,r) when q<1440  -> let (q1,r1) = div q 60 in (0,q1,r1,r)
  |_ -> let (q1,r1) = div q 60 in let (q2,r2) = div q1 24 in (q2,r2,r1,r);;

(sec_en_duree) 86401;;

(*Q.2*)

(*Specification nb_total_sec : Takes a duree and computes its number of seconds:
  Profil= nb_total_sec : int*int*int*int-> seconde
  Semantique= nb_total_sec(j, h, m, s) is the total number of seconds calculated with the equation 
  j jours + h heures + m minutes + s secondes
  Ex et Prop:
                 (i) nb_total_sec(1, 0, 0, 0) = 86400
                (ii) nb_total_sec(0, 1, 0, 0) = 3600
                (iii) nb_total_sec( 0,0 , 1, 30 ) = 90
                (iv) nb_total_sec( 0 , 0, 2, 0 ) = 120 *)



let nb_total_sec (d:int*int*int*int):seconde = let (j,h,m,s) = d in j * 24*3600 + h*3600+ m*60 + s;;
(nb_total_sec) (0,1,2,0);;
(*Q.3*)

(*Realisation vec_en_duree:
  Algorithm : Calculates the number of seconds represented by the vector, then converts it into
  a duree thanks to the function sec_en_duree.
  Implementation:*)
let vec_en_duree (vect:int*int*int*int):duree = let (j,h,m,s) = vect in 
  let sec = j * 24 * 3600 + h*3600 + m*60 + s in sec_en_duree sec;;

(vec_en_duree) (0,0,1160,34);;

(*Q.4*)

let select_j (d:duree):jour = let (j,_,_,_) = d in j;;
(select_j) (5,4,6,4);;
let select_h (d:duree):heure = let (_,h,_,_) = d in h;;
(select_h) (4,7,4,8);;
let select_m (d:duree):minute = let (_,_,m,_) = d in m;;
(select_m) (0,7,4,8);;
let select_s (d:duree):seconde = let (_,_,_,s) = d in s;;
(select_s) (4,7,4,8);;

(*Q.5*)

(*Realisation duree_en_sec:
  Algorithm = Usage of selectors and nb_total_sec
  Implementation:*)

let duree_en_sec (d:duree):seconde = nb_total_sec(select_j d , select_h d, select_m d, select_s d);;
(duree_en_sec) (0,0,2,1);;

(*Q.6*)

(*Realisation of som_duree1:
  Algorithm :We use duree_en_sec and sec_en_duree.
  Implementation :*)
let som_duree1 (d1:duree)(d2:duree) = sec_en_duree (duree_en_sec d1 + duree_en_sec d2);;
(som_duree1) (0,0,2,0) (0,1,1,0);;

(*Realisation of sum_duree2:
  Algorithm : We add the coordinates of both vectors respectively while taking into account
  the excess (if there is) left by the addition.
  Implementation :*)


let sum_secondes (s1:seconde) (s2:seconde) : seconde = s1+s2;;
let sec_bool (s1:seconde) (s2:seconde) : bool = s1+s2<60;;
let sum_minute (s1:seconde)(s2:seconde) (m1:minute) (m2:minute):minute = 
  if sec_bool s1 s2 then m1 + m2
  else m1+m2+1;;
let minute_bool (m1:minute) (m2:minute) : bool =  m1+m2<60;;
let sum_heures (s1:seconde) (s2:seconde) (m1:minute) (m2:minute) (h1:heure) (h2:heure):heure = 
  if sec_bool s1 s2 then if minute_bool m1 m2 
    then h1 + h2 else h1+h2+1 else if minute_bool (m1+1) (m2) then h1+h2 else h1+h2+1;;
let heure_bool (h1:heure) (h2:heure):bool = h1+h2<24;;
let sum_jours (s1:seconde) (s2:seconde) (m1:minute) (m2:minute) (h1:heure) (h2:heure) (j1:jour) (j2:jour) = 
  if sec_bool s1 s2 then if minute_bool m1 m2 then if heure_bool h1 h2 then j1+j2 else 
        j1+j2+1 else if heure_bool (h1+1) h2 then j1+j2 else j1+j2+1 else 
  if minute_bool (m1+1) (m2) then if heure_bool h1 h2 then j1+j2 else j1+j2+1 else 
  if heure_bool (h1+1) (h2) then j1+j2 else j1+j2+1  ;;
let bool_jour (j1:jour) (j2:jour):bool = j1+j2<31;; 

let sum_min_bigger (s1:seconde) (s2:seconde) (m1:minute) (m2:minute) = if sec_bool s1 s2 then m1 + m2 -60 else m1 + m2 -59;;

let sum_hour_bigger (s1:seconde) (s2:seconde) (m1:minute) (m2:minute) (h1:heure) (h2:heure) = 
  if sec_bool s1 s2 then if minute_bool m1 m2 then h1 + h2 -24 else h1 + h2 -23 else
  if minute_bool (m1+1) m2 then h1 + h2 -24 else h1 + h2 -23;;

let sum_day_bigger (s1:seconde) (s2:seconde) (m1:minute) (m2:minute) (h1:heure) (h2:heure) (j1:jour) (j2:jour) = 
  if sec_bool s1 s2 then if minute_bool m1 m2 then 
      if heure_bool h1 h2 then j1 + j2 -31 else j1 + j2 -30 else 
    if heure_bool (h1+1) (h2) then j1+j2 -31 else j1 + j2 -30 else 
  if minute_bool (m1+1) m2 then j1 + j2 -31 else if heure_bool (h1+1) h2 then j1+j2-31 else j1+j2-30;;

let sum_duree2 (d1:duree) (d2:duree) :duree= let (j1,h1,m1,s1) = d1 and (j2,h2,m2,s2) = d2 in 
let sum_sec = (if sec_bool s1 s2 then sum_secondes s1 s2 else (s1+s2) - 60) and 
sum_min = (if minute_bool m1 m2 then if 60 = sum_minute s1 s2 m1 m2 then 0 else sum_minute s1 s2 m1 m2  else sum_min_bigger s1 s2 m1 m2) and 
sum_h = (if heure_bool h1 h2 then if 24 = sum_heures s1 s2 m1 m2 h1 h2 then 0 else sum_heures s1 s2 m1 m2 h1 h2  else 
    sum_hour_bigger s1 s2 m1 m2 h1 h2) and 
sum_j = (if bool_jour j1 j2 then if 31 = sum_jours s1 s2 m1 m2 h1 h2 j1 j2 then 0 else sum_jours s1 s2 m1 m2 h1 h2 j1 j2  else 
    sum_day_bigger s1 s2 m1 m2 h1 h2 j1 j2 ) in 
  (sum_j,sum_h,sum_min,sum_sec);;

(sum_duree2) (23,23,59,56) (2,3,6,7);;

(*Q.7*)

(*Realisation eg_duree1:
  Algorithm: Using the function duree_en_sec.
  Implementation:*)

let eg_duree1 (d1:duree) (d2:duree):bool = duree_en_sec (d1) = (duree_en_sec(d2));;
(eg_duree1) (5,4,3,5) (5,4,3,6);;

(*Realisation eg_duree2:
  Algorithm: Decomposing duree into vectors and comparing them.
  Implementation: *)
let eg_duree2 (d1:duree) (d2:duree) :bool= let (j1,h1,m1,s1) = d1 and (j2,h2,m2,s2) = d2 in 
  (j1,h1,m1,s1) = (j2,h2,m2,s2);;
(eg_duree2) (5,4,3,5) (5,4,3,7);;

(*Realisation eg_duree3:
  Algorithm: Decomposing the vectors using Let.
  Implementation: *)
let eg_duree3 (d1:duree) (d2:duree) :bool= let (j1,h1,m1,s1) = d1 and (j2,h2,m2,s2) = d2 in
  (j1=j2) && (h1=h2) && (m1=m2) && (s1=s2);;
(eg_duree3) (5,4,3,9) (5,4,3,5);;

(*Realisation eg_duree4:
  Algorithm: Using selects functions.
  Implementation: *)
let eg_duree4 (d1:duree) (d2:duree):bool = (select_j d1 = select_j d2) && (select_h d1 = select_h d2) && 
                                           (select_m d1 = select_m d2) && (select_s d1 = select_s d2);;
(eg_duree4) (4,2,7,43) (4,2,4,43);;

(*Q.8*)
(*Realisation inf_duree1
  Algorithm : Using the function duree_en_sec
  Implementation: *)
let inf_duree1 (d1:duree) (d2:duree):bool = (duree_en_sec d1) < (duree_en_sec d2);;
(inf_duree1) (5,4,3,9) (5,4,3,5);;

(*Realisation inf_duree2
  Algorithm : Using nested conditional "if"s.
  Implementation: *)
let inf_duree2 (d1:duree) (d2:duree) :bool = let (j1,h1,m1,s1) = d1 and (j2,h2,m2,s2) = d2 in
  if j1>j2 then true else if j1<j2 then false else 
  if h1 > h2 then true else if h1 < h2 then false else
  if m1 > m2 then true else if m1 < m2 then false else
  if s1>s2 then true else false;;
(inf_duree2) (5,4,7,9) (5,4,3,5);;


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

(* ALL RAFAEL WORK HERE NOT NECESSARY TO KEEP IT LIKE THAT SPECIALLY FOR 1.13 *)
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





(* Exerco\ice 2.12.2 *)

type base10 = int;; (*Since the numbers in the base are character then we say that the type base10 is the same type of character*)
type chiffre = char;; (*Since a number is an integer then we say that the type number is the same as int*)
  
(*Creating the function as we said in question 5*)
let chiffreVbase10 (a:chiffre):base10= (*/!\ a as to be between '0' and '9'*)
  int_of_char(a) - int_of_char('0') ;;
  
let base10Vchiffre (b:base10):chiffre= (*/!\ b as to be between 0 and 9*)
  char_of_int(b + int_of_char('0')) ;;

(*
   Exercice 2.13
   Q1.
*)

(* SPECIFICATION
   
   Profile      carhexVbase16: carhex -> base16
   Semantics    carhexVbase16(c) is the positional value of the hexademcimal 
                elementar character c such that c is an element of the set union
                {'0', ... , '9'} U {'A', ... , 'F'}
*)

(* Q2. *)

(* Definition of hexadecimal characters as char.
   /!\ Note that hexadecimal characters are included on the following set :
   /!\ carhex = {char | char ⊂ {'0', ... , '9'} U {'A', ... , 'F'}} *)
type carhex = char ;;

(* Definition of hexacimal elemental characters positional values
   /!\ Note that the positional values are comprised in the interval :
   /!\ base16 = {int ⊂ ℤ | int ⊂ [0;15]} *)
type base16 = int ;;

(* Q3. *)

(* Implementation of carhexVbase16, described in Q1. *)

let carhexVbase16 (chex: carhex) : base16 =
  if (chex >= '0' && chex <= '9') then
    chiffreVbase10(chex)
  else if (chex >= 'A' && chex <= 'F') then
    int_of_char(chex) - 55
  else
    failwith "Invalid carhex value"
;;

(* Q4. *)

(* Definition of hexadecimal characters as char as a sum type. Constructor
   names mimic the pronunciation of each elemental character.
   /!\ Note that hexadecimal characters are included on the following set :
   /!\ carhex = {char | char ⊂ {'0', ... , '9'} U {'A', ... , 'F'}} *)
type carhex = Zero
              | One   | Two   | Three
              | Four  | Five  | Six
              | Seven | Eight | Nine
              | Car_A | Car_B | Car_C
              | Car_D | Car_E | Car_F 
;;

(* The type base16 does not change as its a representation of integer numbers
   /!\ Note that the positional values are comprised in the interval :
   /!\ base16 = {int ⊂ ℤ | int ⊂ [0;15]} 
type base16 = int ;;   
*)


let carhexVbase16_v2 (chex: carhex) : base16 =
  match chex with
  | Zero  -> 0   
  | One   -> 1  | Two   -> 2  | Three -> 3  
  | Four  -> 4  | Five  -> 5  | Six   -> 6
  | Seven -> 7  | Eight -> 8  | Nine  -> 9
  | Car_A -> 10 | Car_B -> 11 | Car_C -> 12
  | Car_D -> 13 | Car_E -> 14 | Car_F -> 15
;;





(*
   Exercice 2.14
   Q1.
*)

(* Defining the type hexa4 of int type
   /!\ Note that values of hexa4 must be included in the set:
   /!\ hexa4 = {int ⊂ ℤ | int ⊂ [0; 16^(4)-1]} *)
type hexa4 = int ;;

(* Q2. *)

(* Defining the product type rep_hexa4 of carhex. This type represents a 4 
characters hexademximal number.
   /!\ All elements of this product type must be of type carhex, see carhex type
   for its definition *)
type rep_hexa4 = carhex * carhex * carhex * carhex ;;

(* Q3. *)

(* SPECIFICATION

   Profile      ecriture_hex: hexa4 -> rep_hexa4
   Semantics    ecriture_hex(n) is the representation of the integer value n as 
                four hexadecimal elements writing. Note that n must be included 
                in the interval [0; 16^(4)-1]
*)

(* Q4. *)

(* SPPECIFICATION
  
   Profile       base16Vcarhex: base16 -> carhex
   Semantics     base16Vcarhex(b16) is the elemental hexadecimal character 
                 represented by the positional value b16, such that b16 is an
                 integer comprised in the interval [0; 15]
*)
let base16Vcarhex (b16: base16) : carhex =
   match b16 with
   | 0  -> Zero
   | 1  -> One   | 2  -> Two   | 3  -> Three
   | 4  -> Four  | 5  -> Five  | 6  -> Six
   | 7  -> Seven | 8  -> Eight | 9  -> Nine
   | 10 -> Car_A | 11 -> Car_B | 12 -> Car_C
   | 13 -> Car_D | 14 -> Car_E | 15 -> Car_F
   | _  -> failwith "invalid base16 value"
;;

(* Q5. *)

let ecriture_hex (hx4: hexa4) : rep_hexa4 =
   let ten=hx4/16 and u=hx4 mod 16 in
      let hndd=ten/16 and d=ten mod 16 in
         let thsnd=hndd/16 and c=hndd mod 16 in 
            let m=thsnd mod 16 in
   (base16Vcarhex(m), base16Vcarhex(c), base16Vcarhex(d), base16Vcarhex(u))
;;

(* Q6. *)

assert((ecriture_hex(0)) = (Zero, Zero, Zero, Zero)) ;;
assert((ecriture_hex(16*16*16*16 - 1)) = (Car_F, Car_F, Car_F, Car_F)) ;;
assert((ecriture_hex(12345)) = (Three, Zero, Three, Nine)) ;;


