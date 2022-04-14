(*
   Exercice 6.1
   Q1. 
*)

type 'a abin = Empty | Node of 'a*'a abin*'a abin ;;

(* 
   Exercice 6.1.1
   Q2. 
*)

(* Defining sub-trees *)
let leaf13 = Node(13, Empty, Empty) ;;
let leaf33 = Node(33, Empty, Empty) ;;
let node7l = Node(7, Empty, leaf13) ;;
let node7r = Node(7, leaf13, Empty) ;;
(* Defining trees *)
let ex1: int abin = 
  Node(15, node7r, leaf33) ;;
let ex2: int abin =
  Node(15, node7l, leaf33) ;;

(* 
   Exercice 6.1.2
   Q3. 
*)

(* Defining building functions *)
let abS (n: int) : int abin =
  Node(n, Empty, Empty) ;;

let abUNd ((n: int),(a: int abin)) : int abin = 
  Node(n, Empty, a) ;;

let abUNg ((a: int abin),(n: int)) : int abin =
  Node(n, a, Empty) ;;

(*  Q4. *)

(* Redefining trees *)
let ex1 = Node(15, 
  (abUNd 
    (
      7, 
      (abS 13)
    )
  ), (abS 33)
) ;;

let ex2 = Node(15,
  (abUNg
    (
      (abS 13),
      7
    )
  ), (abS 33)
) ;;

(*
   Exercice 6.1.3
   Q5.
*)
let (!) (x:'a) : 'a abin = Node(x, Empty, Empty)  ;;

(* Q6. *)
let (->>) (r:'a) (d:'a abin) : 'a abin = Node(r, Empty, d) ;;

let (<<-) (g:'a abin) (r:'a) : 'a abin = Node(r, g, Empty) ;;

(* Q7. *)
let ex1 = Node(15, (7->>(!13)), !33) ;;

let ex2 = Node(15, ((!33)<<-7), !33) ;;

(* Implementing --> and <-- operators *)
let (-->) (r:'a) (d : 'a abin) : 'a abin -> 'a abin = 
  fun (g : 'a abin) -> Node(r,g,d) ;;

let (<--) (g: 'a abin) (f: 'a  abin -> 'a abin) : 'a abin = 
  f g ;;

(* Q8. *)
let ex1 = (7 ->> !13) <-- 15 --> !33 ;;

let ex2 = (!13 <<- 7) <-- 15 --> !33 ;;

(* 
   Exercice 6.1.4 
*)

let ex12 = ex1 <-- 1 --> ex2 ;;

let rec arbreVchaine_op (nVc: 'a -> string) (a: 'a abin) : string =
  let estSingle (a: 'a abin) : bool =
    match a with Node( _, Empty, Empty) -> true | _ -> false
  in
  let parenth (a: 'a abin) (str_a: string) = 
    if estSingle a then str_a else "(" ^str_a ^")"
  in 
  match a with
  | Empty -> ""
  | Node(r,g,d) -> let str_r = nVc r in
    if g = Empty && d = Empty then
      "!" ^str_r
    else
      let str_g = arbreVchaine_op nVc g
      and str_d = arbreVchaine_op nVc d in
      if g = Empty then
        str_r ^"->>" ^parenth d str_d
      else if d = Empty then
        (parenth g str_g) ^"<<-" ^str_r
      else
        (parenth g str_g) ^"<--" ^str_r ^"-->" ^parenth d str_d
;;

arbreVchaine_op string_of_int ex1;;
(* Output: - : string = "(7->>!13)<--15-->!33" *)
arbreVchaine_op string_of_int ex2;;
(* Output: - : string = "(!13<<-7)<--15-->!33" *)
arbreVchaine_op string_of_int ex12 ;;
(* Output: - : string = "((7->>!13)<--15-->!33)<--1-->((!13<<-7)<--15-->!33)" *)

let affiche_intabin (a : int abin) : unit = 
  Format.printf "@[%s@]" (arbreVchaine_op string_of_int a) 
;;

#install_printer affiche_intabin ;;

#use "affiche_arbre_op.ml" ;;

arbreVchaine_op string_of_int ex1;;
(* Output: - : string = "(7->>!13)<--15-->!33" *)
arbreVchaine_op string_of_int ex2;;
(* Output: - : string = "(!13<<-7)<--15-->!33" *)
arbreVchaine_op string_of_int ex12 ;;
(* Output: - : string = "((7->>!13)<--15-->!33)<--1-->((!13<<-7)<--15-->!33)" *)


(* Q9. *)


-------

(*6.2 Sommes *)
type binary_tree =
  | Empty
  | Node of int * binary_tree * binary_tree

(*Q.1*)

let rec somme_arbre (t:binary_tree) = match t with
  |Empty -> 0
  |Node(a,lt,rt) -> a + somme_arbre lt + somme_arbre rt;; 
let bt1 =
  Node (100,
        Node (30,Empty,Empty),
        Node (74,Empty,Empty)
       )


(*Q.2*)
let ex1 = Node(15,Node (7,Empty,Node(13,Empty,Empty)),Node(33,Empty,Empty));;
let ex2 = Node(15,Node (7,Node(-8,Empty,Empty),Empty),Node(-6,Empty,Empty));;
(somme_arbre) ex1;;
(somme_arbre) ex2;;

(*Q.3*)
(*
somme_arbre <--Node (15, Node (7, Empty, Node (13, Empty, Empty)),Node (33, Empty, Empty))
  somme_arbre <-- Node (33, Empty, Empty)
    somme_arbre <-- Empty
    somme_arbre --> 0
    somme_arbre <-- Empty
    somme_arbre --> 0
  somme_arbre --> 33
  somme_arbre <-- Node (7, Empty, Node (13, Empty, Empty))
    somme_arbre <-- Node (13, Empty, Empty)
      somme_arbre <-- Empty
      somme_arbre --> 0
      somme_arbre <-- Empty
      somme_arbre --> 0
    somme_arbre --> 13
    somme_arbre <-- Empty
    somme_arbre --> 0
  somme_arbre --> 20
somme_arbre --> 68
- : int = 68
*)

(*Q.4*)
let rec somme_pos_neg_0 (t:binary_tree):int*int*int  = match t with
  |Empty -> (0,0,0)
  |Node(a,lt,rt) -> let (pos1,neg1,nul1) = somme_pos_neg_0 lt and (pos2,neg2,nul2) = somme_pos_neg_0 rt in 
    if a>0 then (a+pos1+pos2, neg2+neg1,0) else if a<0 then (pos1+pos2, neg1 + neg2 + a,0) else (pos1+pos2,neg1+neg2,1+a);;     
(somme_pos_neg_0) ex1;;
(somme_pos_neg_0) ex2;;


(*6.3 Arbres symétriques*)

(*Q.1*)

(*
                                                  1
                                                /  \
                                               4    2
                                              / \    \
                                             7   5    3
                                                /
                                               6
*)

(*Q.2*)
let rec sym_arbe (t:binary_tree):binary_tree = match t with
  |Empty -> Empty
  |Node(a,lt,rt) -> Node(a, sym_arbe rt, sym_arbe lt);;

(sym_arbe) ex1;;

(*Q.3*)
let ex3 = sym_arbe ex1;;
let sont_sym (t1:binary_tree) (t2:binary_tree):bool = if t1 = sym_arbe t2 then true else false;;
(sont_sym) ex3 ex1;;

(*Q.4*)
let sont_sym2 (t1:binary_tree) (t2:binary_tree) :bool = let rec transform_arbre_seq (t:binary_tree):int list = match t with
    |Empty -> []
    |Node(a,lt,rt) -> (a:: transform_arbre_seq lt)@(transform_arbre_seq rt)
  in transform_arbre_seq t2 = transform_arbre_seq (sym_arbe t1) ;; 

(sont_sym2) ex1 ex3;;

