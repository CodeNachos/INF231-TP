(* -----------------------------------------------------------------------
   inf201-ABDELKADER-MASCARENHAS-DOKTORCIK-TP6.ml : cr exercices TP no6
   Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr>  \ 
   Rafael MASCARENHAS <Rafael.Mascarenhas@etu.univ-grenoble-alpes.fr>   > Groupe Ocaml_best_camel
   Maxence DOKTORCIK <Maxence.Doktorcik@etu.grenoble-inp.fr>           / 
   ----------------------------------------------------------------------- *)
type flux = int;; (*[-1; +infinity]*)

(*Q.1*)

type releve = V | C of flux*releve;;

(*Q.2*)

(*Specification: nbj_sans returns the number of days where no cars where observed on all days of the list.
  Profile = nbj_sans: releve -> int
  Semantique = (nbj_sans) r takes a list, goes through it and computes the number of times
  an element (flux) of the list r is zero. 
  Examples:
            C(4,C(0,C(5,V))) = 1
            C(0,C(0,C(2,V))) = 2
            C(7,C(-1,C(8,V))) = 0
*)

(*Q.3*)

let r1 = C(3,C(6,C(9,C(6,V))));;
let r2 = C(5,C(0,C(4,V)));;
let r3 = C(3,C(0,C(9,C(0,C(0,C(2,C(0,V)))))));;

(*Q.4*)

let rec nbj_sans (l:releve):int = match l with
  |V -> 0
  |C(a,l) -> if a = 0 then 1 + nbj_sans l else nbj_sans l;;

(*Q.5*)

(*Specification nbj_avec: Returns the number of times x cars where observed on all days of releve.
  Profile= nbj_avec: releve -> int -> int
  Semantique = (nbj_avec r x ) Takes in a list r and a number x and computes the amount of times
  the element x is in the list r.
  Exampples:
      nbj_sans(C(4,C(0,C(1,C(0,V)))) , 4)  = 1
      nbj_sans(C(3,C(-1,C(3,C(3,C(-1,V))))) , 3) = 3
      nbj_sans(C(4,C(0,C(-1,C(5,V)))), 2) = 0
*)

(*Q.6*)

(*  nbj_avec (r x) :
                    {
                      0 if r = V

                      1 + nbj_avec (rprime x ) if r = C (a,rprime) and a = x

                      nbj_avec(rprime x ) if r= C(a,rprime) and a <> x
                    }


*)

(*Q.7*)
let rec nbj_avec (x:flux) (r:releve) : int (* >= 0 *) =
  match x, r with 
  | _, V -> 0 
  | x, C(x,fin) -> 1 + nbj_avec x fin 
  | x, C(pr,fin) -> (* on a donc ici x <> pr *) nbj_avec x fin;;

(*We get the error : "Variable x is bound several times in this matching"
  Which means that when we pattern-match, the variables in each match should only
  appear once. In this case, x appears twice. *)

(*Q.8*)
let rec nbj_avec (x:flux) (r:releve) : int (* >= 0 *) =
  match x, r with 
  | _, V -> 0 
  | (b,C(a,l)) -> if a = b then 1 + nbj_avec x l  else nbj_avec x l;;   


let r4 = C(5,C(0,C(4,C(5,C(2,V)))));;
assert(nbj_avec 6 r1 = 2);;
assert(nbj_avec 5 r4 = 2);;


(*Q.9*)

let nbj_panne (r:releve) = nbj_avec (-1) r;;


(*Q.10*)

assert (nbj_panne r1 = 0);;
assert(nbj_panne r2 = 0);;
assert(nbj_panne r3 = 0);;

(*Q.11*)

(*Specification flux_app = Returns a boolean indicating whether an element is in a given list.

  Profile = flux_app: releve -> flux -> bool

  Semantique = (flux_app x r) is true if and only if x is in the list r.
*)

(*Q.12*)

let rec flux_app (r:releve) (x:flux) = match r with
  | V -> false
  | C(a,rprime) -> a = x || flux_app rprime x;;

(*Q.13 *)

#trace flux_app;;

(*For my constant R3, all elements of my list were compared to 7.*)

(*Q.14*)

(*W ealready saw the condition where x doesn't belong to our list, it compares all elements
  to x until eventually it reaches the end of the list, and returns false. Let us test
  with a case where x does indeed belong to the list:*)

let r3bis = C(6,C(9,C(8,C(7,C(3,C(2,V))))));;

(flux_app) r3bis 7;;

(*We get the following result:
  # (flux_app) r3bis 7;;
  flux_app <-- C (6, C (9, C (8, C (7, C (3, C (2, V))))))
  flux_app --> <fun>
  flux_app* <-- 7
  flux_app <-- C (9, C (8, C (7, C (3, C (2, V)))))
  flux_app --> <fun>
  flux_app* <-- 7
  flux_app <-- C (8, C (7, C (3, C (2, V))))
  flux_app --> <fun>
  flux_app* <-- 7
  flux_app <-- C (7, C (3, C (2, V)))
  flux_app --> <fun>
  flux_app* <-- 7
  flux_app* --> true
  flux_app* --> true
  flux_app* --> true
  flux_app* --> true
  - : bool = true;;
  As we can see, it didn't compare all elements of the list, it stopped when an element was indeed
  equal to our flux, this is a consequence of the "or" operator.*)

(*Q.15*)

let rec general_app (seq:'a list) (x:'a) = match seq with
  | [] -> false
  | a::l -> a = x || general_app l x ;;

type relevenv = releve (* It is never empty*)

(*Q.17*)

(*Specification flux_min : Returns the minimum element of a given list.
  Profile = flux_min: relevenv -> flux

  Semantique = (flux_min r) Compares an element in r with the following one, and computes
  the smallest element of the list. (Excluding -1 obviously)

  Examples =
            C(4,C(7,C(3,C(8,C(1,C(7,...)))))) (We suppose that no element afterwards is 
            smaller than 1) = 1

            C(6,C(2,C(9,C(-1,C(7,...))))) (We suppose that no element afterwards is smaller
            than 2) = 2
*)

(*Q.18*)

let rec flux_min (l:relevenv):flux = match l with
  |C(-1,l) -> flux_min l 
  |C(a,l) -> min (a) (flux_min l);;  


(*Q.19*)

(*We get a warning:
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  V
  It means that a case is missing in our pattern-matching. This warning happens
  because we don't match V, however, we normally shouldn't expect to receive
  this error, since our type releveVN is defined without V. We could suppress this warning by
  matching with a general case where we return an error or some value.  
*)

(*Q.20*)

type releveVN2 = S of flux | C of flux*releveVN2;;

(*Q.21*)

let rec fluxmax (r:releveVN2) : flux = match r with
  | S x -> x 
  | C(a,rprime) -> max (a) (fluxmax rprime);;

let r5 = C(5,C(3,C(9,C(5,S(10)))));;


(*Q.22*)

let fluxobs (j:int) (r:releveVN2):flux = let rec acc (j:int) (r:releveVN2) (i:int):flux = 
 match r with
| S x -> if i = j then x else failwith "Error, this j doesn't exist"
| C(a,rprime) -> if i <> j then acc (j) (rprime) (i+1) else a
  in acc j r 1;;      
