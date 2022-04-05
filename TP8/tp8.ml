
(* -----------------------------------------------------------------------
   inf201-ABDELKADER-MASCARENHAS-DOKTORCIK-TP8.ml : cr exercices TP no8
   Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr>  \ 
   Rafael MASCARENHAS <Rafael.Mascarenhas@etu.univ-grenoble-alpes.fr>   > Groupe Ocaml_best_camel
   Maxence DOKTORCIK <Maxence.Doktorcik@etu.grenoble-inp.fr>           / 
   ----------------------------------------------------------------------- *)
(*TP8 : Somme d'une suite de nombres*)

(*Q.1*)
type chiffre = int (*Only restricted to numbers from [[0 ; 10[[ *);;
type chiffrecode = char (*Only retricted to characters going from '0' to '9' *);;
type nombre = chiffrecode list;;
type txtnb = char list (*Only characters from '0' to '9' and the character space ' ' *);;

(*Q.2*)

(*Specification somme_txtnb : Calculates the sums of numbers of txtnb
  Profile = somme_txtnb: txtnb -> int
  Semantics = (somme_txtnb ltxt) Finds the numbers in a the list ltxt and computes their sum.
  Examples =
            somme_txt ['5';'7';'6'; ' '; '7'; '3'; ' '; '5'] = 654
            somme_txt ['4';'2'; ' '] = 42
            somme_txt ['4';'2';' '; '0';] = 42

*)

(*Specification les_nb : Constructs a sequence containing the numbers present in txtnb
  Profile = les_nb : txtnb -> nombre list 
  Semantics = (les_nb ltxt) Constructs a list containing lists of the numbers of ltxt.
    Example:
              (les_nb) ['4';'2';' '; ' '; '4'; ' '; '2'] = [['4';'2'] ; ['4'] ; ['2'] ]
              (les_nb) ['5';'0';'0';' ';'4';'0'; ' '; ' '; '2'; ' '] = [['5';'0';'0'];['4';'0'];['2']]
              (les_nb) ['4';' ';' '; ' ';'2';' '; ' ']= [['4'];['2']]
*)


(*Specification snbVnat : Constructs an integer list from a nombre list
  Profile = snbVnat : nombre list -> int list
  Semantique = (snbVnat nombre_l) Constructs a list containing the integers of nombre_l.
  Exampple :
            (snbVnat) [['4';'2'] ; ['2';'4']] = [42 ; 24]
            (snbVnat) [['4'];['2']] = [4;2]
            (snbVnat) [['4';'2';'4';'2']; ['4'] ; ['2'] ; ['4';'1'; '1']] = [4242;4;2;411]
*)

(*Specification somme : Sums the integers of a given list
  Profile = somme : int list -> int
  Semantics = (somme l_int) Computes the sum of the elements of l_int
  Examples:
            (somme) [22;11;7;2] = 42
            (somme) [30;12] = 42
            (somme) [1;2;3;4;5;6;6;7;8;] = 42

*)

(*Q.3*)
let rec somme (l: int list) :int = match l with
  |[] -> 0
  |a::lp -> a + somme lp;;
assert (somme [1;2;3;4;5;6;6;7;8] = 42);;
assert ((somme) [22;11;7;2] = 42);;
assert ((somme) [30;12] = 42);;
(*Q.4*)

let ccVc (c: chiffrecode):chiffre = (int_of_char c) - (int_of_char '0');;

assert (ccVc '4' = 4);;
assert(ccVc '2' = 2);;
assert(ccVc '6' = 6);;
(*Specification power = Computes the power of an int
  Profile = power : int -> int -> int
  Semantics = (power a n) Computes an integer which represents a to the power of n.
  Examples = 
              power 2 4 = 16
              power 9 2 = 81
              power 42 1 = 42*)

let rec power (a:int) (n:int) :int = if n = 0 then 1 else a* power a (n-1);;

let rec nbVnat (lnombre:nombre):int = match lnombre with
  |[] -> 0
  | a::lp -> let n = List.length (lnombre) in let pos = power (10) (n-1) in (ccVc a)*pos  + nbVnat lp;;

(nbVnat) ['4'; '2';'0'];;

let rec snbVnat (l:nombre list):int list = match l with
  |[] -> []
  |a::lp -> let list_int = nbVnat a in list_int :: (snbVnat lp);;

assert ((snbVnat) [['4'];['2']] = [4;2]);;
assert ((snbVnat) [['4';'2';'4';'2']; ['4'] ; ['2'] ; ['4';'1'; '1']] = [4242;4;2;411]);;
assert ((snbVnat) [['4';'2'] ; ['2';'4']] = [42 ; 24]);;

(*Q.5*)

let rec sup_esp (l:txtnb):txtnb = match l with
  |[] -> []
  | a:: lp -> if a = ' ' then sup_esp lp else a:: lp;;

assert (sup_esp ['5';'7';'6'; ' '; '7'; '3'; ' '; '5'] = ['5';'7';'6'; ' '; '7'; '3'; ' '; '5']);;

let prnb_r (lp:txtnb): nombre*txtnb = let rec prnb_r2 (l:txtnb) (i:int) = if i = 1 then let no_begin_spaces = sup_esp l in match no_begin_spaces with 
  |[] -> ([],[])
  | a:: lpp -> if a <> ' ' then let (nbr,reste) = prnb_r2 (lpp) (i+1) in (a::nbr,reste) else ([], lpp)
    else
      match l with
      |[] -> ([],[])
      | a:: lpp -> if a <> ' ' then let (nbr,reste) = prnb_r2 (lpp) (i) in (a::nbr,reste) else ([],a::lpp)
  in 
  prnb_r2 lp 1;;

assert((prnb_r) [' ';' ';' ';'5';'7';'6'; ' '; '7'; '3'; ' '; '5']= (['5'; '7'; '6'], [' ';'7'; '3'; ' '; '5']));;
assert((prnb_r) [' ';' ';'4';'2';' ';'5';'6';' ';'8'] = (['4'; '2'], [' '; '5'; '6'; ' '; '8'])) ;;
assert((prnb_r) [' '; '3';'6';' ';' '; '5'; '6'; '3';] = (['3';'6'], [' ';' '; '5'; '6'; '3';]));;


let rec les_nb (lnombre:txtnb):nombre list = match lnombre with
  |[] -> []
  |a::lp -> let (first_num,rest) = prnb_r lnombre in first_num :: les_nb rest;;
(les_nb) ['5';'0';'0';' ';'4';'0'; ' '; ' '; '2'; ' '];;

(*Q.6*)

let somme_txt (lnombre:txtnb):int = let my_numbs = les_nb lnombre in let list_ints = snbVnat my_numbs in somme list_ints;; 
somme_txt ['4';'2'; ' '];;
