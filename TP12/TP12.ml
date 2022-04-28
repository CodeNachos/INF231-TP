(* -----------------------------------------------------------------------
   inf201-ABDELKADER-MASCARENHAS-DOKTORCIK-TP9.ml : cr exercices TP no9
   Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr>  \ 
   Rafael MASCARENHAS <Rafael.Mascarenhas@etu.univ-grenoble-alpes.fr>   > Groupe Ocaml_best_camel
   Maxence DOKTORCIK <Maxence.Doktorcik@etu.grenoble-inp.fr>           / 
----------------------------------------------------------------------- *)

(* Exercice 6.4 *)
type ’a abr =
| Av
| Ab of ’a abr (* ABR *) * ’a * ’a abr ;;(* ABR *)

(* Q.1 *)
let rec insert (e:int) (a:'a abr) : 'a abr = match a with
    | Av -> Ab(Av,e,Av)
    | Ab(left,elt,right) -> if e<=elt then Ab(insert (e) (left),elt,right) else Ab(left,elt,insert (e) (right));;

insert 3 (Ab(Ab(Ab(Av,1,Av),2,Ab(Av,4,Av)),5,Ab(Av,9,Av)));; 
(* Gives Ab (Ab (Ab (Av, 1, Av), 2, Ab (Ab (Av, 3, Av), 4, Av)), 5, Ab (Av, 9, Av)) *)


(* Q.2 *)
let rec parcours_sym (a:'a abr) : 'a list = match a with 
  | Av -> []
  | Ab(left,elt,right) -> (parcours_sym left)@[elt]@(parcours_sym right);; 

parcours_sym (Ab(Ab(Ab(Av,1,Av),2,Ab(Av,4,Av)),5,Ab(Av,9,Av)));; (* Gives [1;2;4;5;9] *)

(* Q.3 *)
let rec seqVarbre (s:'a list):'a abr = match s with 
  | [] -> Av 
  | elt::sp -> insert elt (seqVarbre sp);;

seqVarbre [2;1;10;3;5];; (* Gives Ab (Ab (Ab (Av, 1, Ab (Av, 2, Av)), 3, Av), 5, Ab (Av, 10, Av)) *)

(* Q.4 *)
let seqVarbre (s:'a list) : 'a abr = 
  List.fold_right (fun e acc -> insert e acc) s Av;;

seqVarbre [2;1;10;3;5];; (* Gives Ab (Ab (Ab (Av, 1, Ab (Av, 2, Av)), 3, Av), 5, Ab (Av, 10, Av)) *)

(* Q.5 *)
let liste_trie (l:'a list) : 'a list =
  parcours_sym (seqVarbre l);;

liste_trie [2;1;10;3;5];; (* Gives [1;2;3;5;10] *)

(* Exercice 6.5 *)
(* Q.1 *)
type operation =  Sum | Min | Mult | Div;;
type operande = float ;;
type expr = 
  | Av of operande 
  | Ab of expr*operation*expr;;

(* Q.2 *)
let expr1 = Ab(Av(1.),Sum,Ab(Av(2.),Mult,Av(3.)));;
let expr2 = Ab(Ab(Av(1.),Sum,Av(2.)),Mult,Av(3.));;

(* Exercice 6.5.1 *)
(* Specification : vrai_op
   Profil : operation -> (float -> float -> float)
   Semantic : vrai_op op will associate the real function of the name of op
   Examples: vrai_op Sum = (+.)
             vrai_op Min = (-.) 
  *) 
let vrai_op (op:operation):(float -> float -> float) = match op with 
  | Sum -> ( +. ) 
  | Min -> ( -. )
  | Mult -> ( *. ) 
  | Div -> ( /. ) ;;

(* Q.3 *)
let rec val_expr (a:expr) : float = match a with 
  | Av(e) -> e
  | Ab(left,op,right) -> (vrai_op(op)) (val_expr(left)) (val_expr(right));;

(* Q.4 *)
val_expr(expr1);; (* Gives 7 *)
val_expr(expr2);; (* Gives 9 *)

(* Exercice 6.5.2 *)
(* Q.5 *)
let convert ()

