(* -----------------------------------------------------------------------
   inf201-ABDELKADER-MASCARENHAS-DOKTORCIK-TP3.ml : cr exercices TP no5
   Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr>  \ 
   Rafael MASCARENHAS <Rafael.Mascarenhas@etu.univ-grenoble-alpes.fr>   > Groupe Ocaml_best_camel
   Maxence DOKTORCIK <Maxence.Doktorcik@etu.univ-grenoble-alpes.fr>    / 
----------------------------------------------------------------------- *)

(* 
   Exercice 4.1
   Q1. 
*)

(* Le type flux represente le nombre de vehicules qui sont passes pendant la
   periode d'un jour. C'est donc le flux journalier
   /!\ Note that type ∈ N ∪ {-1} where -1 indique la panne du compteur *)
type flux = int ;;

(* Le type releve represent la sequance d'observations sur une periode
   d'observation. Chaque element correspond à un flux journalier de type flux *)
type releve = V | C of flux*releve ;;

(* Q2. *)

(* SPECIFICATION nbj_sans returns the number of days in which no vehicles passed
                 by the observation point

   Profile : nbj_sans : releve -> int
   
   Semantics : (nbj_sans r ) = x
               Providing a releve of a certain set of days (r) the function will
               iterate through the recursive list r and count the name of days 
               in which the flux is 0 and returning this value (x)
  
   Exemples : (i)   (nbj_sans C(4, C(8, C(1, C(3, V))))) = 0
              (ii)  (nbj_sans C(4, C(0, C(1, C(0, V))))) = 2 
              (iii) (nbj_sans C(4, C(0, C((-1), C(5, V))))) = 1         
*)

(* Q3. *)

let r1 = C(4, C(8, C(1, C(3, V)))) ;;             (* No null flux day *)
let r2 = C(4, C(0, C(69, C(5, V)))) ;;          (* One null flux day *)
let r3 = C(0, C(3, C(0, C(42, C(1,C(0, V)))))) ;;  (* Many null flux days *)

(* Q4. *)

let rec nbj_sans (r:releve) : int = 
  match r with
  | V -> 0
  | C(_,rp) -> 1 + (nbj_sans rp) ;;

(* Q5. *)

(* SPECIFICATION nbj_avec calculates the number of days a given number x of 
                 vehicles crossed the the observation point during the given 
                 observation period

   Profile :   nbj_avec : flux -> releve -> int

   Semantics : (nbj_avec x r) = n
                Recursively interating trough the releve (r) provided, it counts
                the nomber of instances where the daily flux is equal to a given
                flux (x)
    
   Exemples : 
*)

(* Q6. *)

(* 
                     | 0                        if r = V
   nbj_avec (x, r) : | 1 + nbj_avec(x, r')      if r=C(f,r') and f=x
                     | nbj_avec(x, r')      if r=C(f,r')
*)
              
(* Q7. *)

(*
let rec nbj_avec (x:flux) (r:releve) : int (* >= 0 *) =
  match x, r with
  | _, V -> 0
  | x, C(x,fin) -> 1 + nbj_avec x fin
  | x, C(pr,fin) -> (* on a donc ici x <> pr *) nbj_avec x fin

Interpreter output :
72 |   | x, C(x,fin) -> 1 + nbj_avec x fin
              ^
Error: Variable x is bound several times in this matching

The interpreter rises an error as the variable x is redefined more than once 
inside the same match statement
*)

(* Q8. *)

let rec nbj_avec (x:flux) (r:releve) : int (* >= 0 *) =
  match r with
  | V -> 0
  | C(pr,fin) when pr=x -> 1 + (nbj_avec x fin)
  | C(pr,fin) -> nbj_avec x fin                     (* on a donc ici x <> pr *) 

(* Q9. *)

let nbj_panne (r:releve) : int = nbj_avec (-1) r;;

(* Q10. *)

assert((nbj_panne r1) = 0) ;;
assert((nbj_panne r2) = 0) ;;
assert((nbj_panne r3) = 0) ;;

(* Q11. *)

(* SPECIFICATION flux_app returns true if a certain given flux is present in a 
                 given releve

   Profile :   flux_app : flux -> releve -> bool

   Semantics : (flux_app x r)=true
               if and only if when recursively iterating trough r a flux of any
               instance of r is equal to x

   Exemples :

*)

(* Q12. *)

let rec flux_app (x:flux) (r:releve) : bool =
  match r with
  | V -> false
  | C(f,rp)-> f=x || (flux_app x rp)
;;

(* Q13 *)

(* max did it *)

(* Q14. *)

(* Youssef did it *)

(* Q15. *)

let rec app (x:'a) (r:'b list) : bool =
  match r with
  | [] -> false
  | a::l -> a=x || (app x l) 
;;
