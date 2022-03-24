(* -----------------------------------------------------------------------
   inf201-ABDELKADER-MASCARENHAS-DOKTORCIK-TP6.ml : cr exercices TP no7
   Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr>  \ 
   Rafael MASCARENHAS <Rafael.Mascarenhas@etu.univ-grenoble-alpes.fr>   > Groupe Ocaml_best_camel
   Maxence DOKTORCIK <Maxence.Doktorcik@etu.grenoble-inp.fr>           / 
----------------------------------------------------------------------- *)

(* 
   Exercice 4.2 
   Q1.
*)

(* The monome type is a implementation of monomials using a prduct type 
   such that monome = ℤ * N 
   /!\ Note that the coeeficient is any integer number, and the power of the 
   monomial is a natural number *)
type monome = int*int ;;

(* The polynome type implements polynimials using a list of momomials. Here the 
   constant polynomial 0 is represented by the empty list *)
type polynome = monome list ;;

(* Q2. *)
(* a) *)
let constM1 :monome = (10,0) ;;                                  (* m1=  10   *)
let constM2 :monome = (7,1) ;;                                   (* m2=  7x   *)
let constM3 :monome = ((-3),2) ;;                                (* m3= -3x^2 *)
let constM4 :monome = (1,4) ;;                                   (* m4=  x^4  *)
let constM5 :monome = ((-1),4) ;;                                (* m5= -x^4  *)

(* b) *)
let p1 :polynome = [constM1] ;;                        (* p1=  10             *)
let p2 :polynome = [constM2; constM1] ;;               (* p2=  7x + 10        *)
let p3 :polynome = [constM3; constM2] ;;               (* p3= -3x^2 + 7x      *)
let p4 :polynome = [constM4; constM3; constM2];;       (* p4=  x^4 -3x^2 + 7x *)
let p5 :polynome = [constM5] ;;                        (* p5= -x^4            *)

(* Q3 *)

(* Implementation derivMono

  SPECIFICATION
  Profile:    derivMono : monome -> monome
  Semantics:  derivMono(m) is the derivative of the monomial m. By convention, 
              the power of the null monomial is 0.
  Ex:         (i) derivMono(x^4) = 4x^3
*)
let derivMono ((c, p):monome) : monome =
  if (c = 0 || p = 0) then (0, 0) else (c*p, p-1) ;;

(* Q4. *)

(* Implementation of derivPoly

   SPECIFICATION
   Profile:   derivPoly : polynome -> polynome
   Semantics: derivPoly(m) is the derivative of m
   Ex:        (i) derivPoly(x^4 -3x^2 + 7x) = 4x^3 -6x + 7
*)
let rec derivPoly (p:polynome) : polynome =
  match p with
  | []      -> []
  | m::pl -> let (c,pw)=m in 
                (* if it is the null monome we skip it *)
                if (c = 0 || pw  = 0) then derivPoly pl
                else (derivMono m)::(derivPoly pl)
;;


(* Q5. *)

(* Tests with [constM1] to [constM5] *)
assert((derivPoly [constM1]) = []) ;;
(* Output: - : unit = () *)
assert((derivPoly [constM2]) = [(7,0)]) ;;
(* Output: - : unit = () *)
assert((derivPoly [constM3]) = [(-6,1)]) ;;
(* Output: - : unit = () *)
assert((derivPoly [constM4]) = [( 4,3)]) ;;
(* Output: - : unit = () *)
assert((derivPoly [constM5]) = [(-4,3)]) ;;
(* Output: - : unit = () *)

(* Test with p2, p3, p4 *)
assert((derivPoly p2) = [(7,0)]) ;;
(* Output: - : unit = () *)
assert((derivPoly p3) = [(-6,1); (7,0)]) ;;
(* Output: - : unit = () *)
assert((derivPoly p4) = [(4,3); (-6,1); (7,0)]) ;;
(* Output: - : unit = () *)


(* Q6. *)

(* Implementation of sommePoly 

   SPECIFICATION
   Profile:   sommePoly : polynome -> polynome -> polynome
   Semantics: (sommePoly p1 p2) is the sum  of the polynomials p1 and p2, 
              It only adds coefficients if they have the same power, else it 
              compares the powers of the current terms and call recursively the 
              function in a way to match the power of the current terms while 
              adding the highest monomial to the output (1)
   Ex:        (i) (sommePoly (x^4 -3x^2 + 7x) (x^4)) = -3x^2 + 7x
*)
let rec sommePoly (p1:polynome) (p2: polynome) : polynome = 
  match p1, p2 with
  | [], [] -> [] | _, [] -> p1 | [], _ -> p2      (* if p1 and/or p2 are null *)
  | (m1::pl1), (m2::pl2) -> let (c1,pw1)=m1 and (c2,pw2)=m2 in
      if pw1 > pw2 then m1::(sommePoly pl1 (m2::pl2))
      else if pw1 < pw2 then m2::(sommePoly (m1::pl1) pl2)
      else let csum = c1 + c2 in
        if csum = 0 then sommePoly pl1 pl2
        else  (csum,pw1)::(sommePoly pl1 pl2)
;;


(* Q7. *)

(* Test de sommePoly *)
assert((sommePoly p1 p1) = [(20, 0)]) ;;
(* Output: - : unit = () *)
assert((sommePoly p1 p2) = [(7, 1); (20, 0)]) ;;
(* Output: - : unit = () *)
assert((sommePoly p2 p1) = [(7, 1); (20, 0)]) ;;
(* Output: - : unit = () *)
assert((sommePoly p4 p5) = (sommePoly p5 p4) && 
       (sommePoly p4 p5) = p3 && 
       (sommePoly p5 p4) = p3) ;;
(* Output: - : unit = () *)


(* Q8 *)

(* Verifiation that the derivative of the sum is the sum of the derivatives *)
assert((sommePoly (derivPoly p1) (derivPoly p1))=derivPoly (sommePoly p1 p1)) ;;
(* Output: - : unit = () *)
assert((sommePoly (derivPoly p1) (derivPoly p2))=derivPoly (sommePoly p1 p2)) ;;
(* Output: - : unit = () *)
assert((sommePoly (derivPoly p2) (derivPoly p2))=derivPoly (sommePoly p2 p2)) ;;
(* Output: - : unit = () *)
assert((sommePoly (derivPoly p1) (derivPoly p3))=derivPoly (sommePoly p1 p3)) ;;
(* Output: - : unit = () *)
assert((sommePoly (derivPoly p1) (derivPoly p5))=derivPoly (sommePoly p1 p5)) ;;
(* Output: - : unit = () *)
assert((sommePoly (derivPoly p4) (derivPoly p5))=derivPoly (sommePoly p4 p5)) ;;
(* Output: - : unit = () *)
