(* 
   Exercice 4.3
   Q1. 
*)

(* Definition of types *)
(* Chiffres representing integers
   /!\ Note that chiffre ∈ {1,2,3,4,5,6,7,8,9} *)
type chiffre = int ;;

(* chiffCar represents characters representing integers
   /!\ Note that chiffreCar ∈ {’0’,’1’,’2’,’3’,’4’,’5’,’6’,’7’,’8’,’9’} *)
type chiffreCar = char ;;

(* nombre is a sequence of chiffreCar, the empty element is [] *)
type nombre = chiffreCar list ;;

(* txtnb represents a number in base 10 in text format, it is a list of 
   characters. /!\ Note that each element is either an instance of chiffreCar or
   the character ' ' (space) *)
type txtnb = char list ;;


(* Q2. *)

(* SPECIFICATION (somme_txtnb) calculates the sum of numbers represented as an
                 instace of txtnb
    Profile:     somme_txtnb : txtnb -> int
    Semantics:   (somme_txtnb s) separates and translates the numbers represented 
                 as text by s to integer values, then computes and returns its sum
    Ex:          (i) (somme_txtnb [’1’; ’2’; ’3’; ’ ’; ’ ’; ’4’; ’5’; ’ ’; ’6’])
                     = 174
*)

(* SPECIFICATION (les_nb) builds the sequence of numbers (type nombre) 
                 represented as an instance of txtnb 
   Profile:      les_nb : txtnb -> nombre list
   Semantics:    (les_nb s) is a sequence containing each number of s separated 
                 by a space (' ' character), each number is a sequence of 
                 chiffreCar instances
   Ex:           (i) (les_nb [’1’; ’2’; ’3’; ’ ’; ’ ’; ’4’; ’5’; ’ ’; ’6’])
                     = [[’1’; ’2’; ’3’] ; [’4’; ’5’] ; [’6’]]
*)

(* SPECIFICATION (snbVsnat) builds the integer sequence corresponding to a 
                 a sequence of numbers, each number being an instance of nombre
   Profile:      snbVsnat : nombre list -> int list
   Semantics:    (snbVsnat n) is the sequence of integers represented by the 
                 number sequence n (of type nombre list)
   Ex:           (i) (snbVsnat [[’1’; ’2’; ’3’] ; [’4’; ’5’] ; [’6’]])
                     = [123 ; 45 ; 6]                  
*)

(* SPECIFICATION (somme) computes the sum of a sequence of integers
   Profile:      somme : int list -> int
   Semantics:    (somme li) is the sum of the integer numbers of the sequence li
   Ex:           (i) (somme [123 ; 45 ; 6]) = 174   

*)

(* 
   Exercice 4.3.1
   Q3.
*)

let rec somme (li: int list) : int =
  match li with
  | []   -> 0
  | n::r -> n + (somme r)
;;

(* 
   Exercice 4.3.2
   Q4.
*)

let ccVc (cc:chiffreCar) : chiffre =
  match cc with
  | '0' -> 0 | '1' -> 1 | '2' -> 2
  | '3' -> 3 | '4' -> 4 | '5' -> 5
  | '6' -> 6 | '7' -> 7 | '8' -> 8
  | '9' -> 9 |  _  -> failwith "Invalid chiffreCar value" ;;

let nbVsnat (nb: nombre) : int =
  let rec build_nb (nb:nombre) : int*int = 
    match nb with
    | [] -> 0,1
    | c::r -> let (s,p)=build_nb r in (s+(ccVc c)*p), p*10
  in fst (build_nb nb) ;;

let rec snbVsnat (nbl: nombre list) : int list = 
  match nbl with
  | [] -> []
  | n::r -> (nbVsnat n)::snbVsnat r
(* 
   Exercice 4.3.3
   Q5.
*)

let rec sup_esp (txt: txtnb) : txtnb =
  match txt with
  | [] -> []
  | c::r    when c = ' '     -> sup_esp r
  | c::r (* when c != ' ' *) -> txt
;;

let prnb_r (txt:txtnb) : nombre*txtnb =
  let rec sep_txt (txt:txtnb) (acc: nombre) : nombre*txtnb = 
      match txt with
      | [] -> acc,[]
      | c::r when c <> ' ' -> sep_txt r (acc@[c])
      | c::r -> acc,txt
  in sep_txt (sup_esp txt) [] 
;;


let rec les_nb (s:txtnb) : nombre list = 
  let (nb,r)=prnb_r s in
  if nb = [] then [] else nb::les_nb r
;;

(* Q6. *)
let somme_txtnb (s:txtnb) : int = somme (snbVsnat (les_nb s)) ;;