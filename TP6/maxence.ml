(*Exercice 4.1*)
(*Q1*)
type flux = int ;;(*Only positive integers and -1*)
type releve = V | C of flux*releve ;; (*This is a list of releve such as it is defined in the poly*)

(*Exercice 4.1.1*)
(*Q2*)
(*Specification: nbj_sans
  Profil: releve -> int 
  Semantics: nbj_sans(r) is the number of day where there was a nul flow in r.
  Examples:
  nbj_sans(C(4,C(0,C(1,C(0,V)))))  = 2
  nbj_sans(C(4,C(0,C(-1,C(5,V))))) = 1
  nbj_sans(C(4,C(4,C(-1,C(5,V))))) = 0
*)

(*Q3*)
let r1=C(4,C(8,C(1,C(3,V))));;
let r2=C(4,C(0,C(9,C(5,V))));;
let r3=C(4,C(0,C(0,C(5,C(3,V)))));;

(*Q4*)
let rec nbj_sans (r:releve):int=match r with
  |V -> 0
  |C(e,rprime) -> if e=0 then 1+(nbj_sans (rprime)) else nbj_sans(rprime);;

assert(nbj_sans(r1)=0);;
assert(nbj_sans(r2)=1);;
assert(nbj_sans(r3)=2);;

(*Q5*)
(*Specification : nbj_avec returns the number of days where x cars were observed on the whole releve
  Profil: int -> releve -> int
  Semantics: nbj_avec(r,x) is the number of day where x cars were observed in the list r.
  Examples:
  nbj_sans(C(4,C(0,C(1,C(0,V)))) , 4)  = 1
  nbj_sans(C(3,C(-1,C(3,C(3,C(-1,V))))) , 3) = 3
  nbj_sans(C(4,C(0,C(-1,C(5,V)))), 2) = 0
  *)

(*Q6*)

(*Q7*)
(*let rec nbj_avec (x:flux) (r:releve) : int (* >= 0 *) = 
  match x, r with 
    | _, V -> 0 
    | x, C(x,fin) -> 1 + nbj_avec x fin 
    | x, C(pr,fin) -> (* on a donc ici x <> pr *) nbj_avec x fin;;*)

(*The error it gives is (Error: Variable x is bound several times in this matching)
which states that we used multiple x so the same variable which is impossible, we can only match with 1 variable*)

(*Q8*)
let rec nbj_avec (x:flux) (r:releve) : int (* >= 0 *) = 
  match x, r with 
    | _, V -> 0 
    | (b,C(a,fin)) -> if a=b then 1 + nbj_avec x fin else nbj_avec x fin ;;

assert(nbj_avec 2 r1 = 0);;
assert(nbj_avec 4 r2 = 1);;
assert(nbj_avec 0 r3 = 2);;

(*Q9*)
let nbj_panne (r:releve):int = nbj_avec (-1) r;;

(*Q10*)
assert(nbj_panne r1 = nbj_panne r2 = (nbj_panne r3 = 0));;

(*Q11*)
(*Specification flux_app: Returns a boolean that is true if there is at least one day where the given flow appears in the list
  Profil: releve -> flux -> boolean
  Semantics: flux_app(x,r) is true if and only if the element x belongs to r
  Examples:
  nbj_sans(C(4,C(0,C(1,C(0,V)))) , 4)  = true
  nbj_sans(C(3,C(-1,C(3,C(3,C(-1,V))))) , 3) = true
  nbj_sans(C(3,C(-1,C(3,C(3,C(-1,V))))) , 2) = false
  *)

(*Q12*)
let rec flux_app (r:releve) (f:flux):bool = match r with
  | V -> false
  | C(e,rprime) -> e=f || (flux_app rprime f);; 

(*Q13*)
#trace flux_app;;
flux_app r3 7;;
#untrace flux_app;;
(*For my constant r3, all of the elements where compared to 7*)

(*Q14*)

(*Q15*)
let rec app_polymorphe (seq:'a list) (x:'a):bool = match seq with
  | [] -> false
  | e::seqprime -> e=x || (app_polymorphe seqprime x);; 

(*Q16*)
type releveNV = releve;; (*Without V so never empty*)

(*Q17*)
(*Specification fluxmax : Returns the maximum of a given list 
  Profil: releveNV -> flux
  Semantics: fluxmax (r) compares all the element of r to find the max
  Examples:
  nbj_sans(C(4,C(0,C(1,C(0,...)))))  = 4
  nbj_sans(C(3,C(-1,C(3,C(3,C(-1,...)))))) = 3
  nbj_sans(C(-1,C(-1,C(-1,C(-1,C(-1,..)))))) will show an error (that we created with failwith) since it means that the machine was broke all the time so there is no max*)

(*Q18*)
let rec fluxmax (r:releveNV):flux =match r with
  |C(e,rprime) -> max (e) (fluxmax rprime);;

(*Q20*)
type releveNV2 = S of flux | C of flux*releveNV2

(*Q21*)
let rec fluxmax (r:releveNV2):flux =match r with
  |S(x) -> x
  |C(e,rprime) -> max (e) (fluxmax rprime) ;;

(*Q22*)
let fluxobs (j:int) (r:releveNV2):flux = 
  let rec fluxobs_b (j:int) (r:releveNV2) (acc:int):flux =
    match r with
      |S x -> if acc=j then x else failwith "This j doesn't exist"
      |C(e,rprime) -> if j=acc then e else fluxobs_b (j) (rprime) (acc+1) in fluxobs_b j r 1;;

fluxobs (2) ((C(2,C(8,C(9,S 4)))));;
