(* -----------------------------------------------------------------------
   inf201-ABDELKADER-MASCARENHAS-DOKTORCIK-TP9.ml : cr exercices TP no9
   Youssef ABDELKADER <Youssef.Abdelkader@etu.univ-grenoble-alpes.fr>  \ 
   Rafael MASCARENHAS <Rafael.Mascarenhas@etu.univ-grenoble-alpes.fr>   > Groupe Ocaml_best_camel
   Maxence DOKTORCIK <Maxence.Doktorcik@etu.grenoble-inp.fr>           / 
----------------------------------------------------------------------- *)


(* 
   Exercice 5.1
   Q1.
*)
let add = fun (a,b) -> a+b;;

(* Q2. *)
add (3,5) ;;
(* Output: - : int = 8 *)
add (5,3) ;;
(* Output: - : int = 8 *)
(* The following call raises an error because the function add expects a product 
type but a int value was given. In this case curryfication does not work as a
product type is an atomic object that can not be decomposed

add 3;;

Output: 
 Error: This expression has type int but an expression was expected of type 
 int * int *)

 (* Q3. *)
 let addc = fun a b -> a+b ;;

 addc 3 5 ;;
 (* Output: - : int = 8 *)
 addc 5 3 ;;
(* Output: - : int = 8 *)
 addc 3 ;;
(* Output: - : int -> int = <fun> *)

(* This new version, taling two parameters instead of one, does the same 
computation as we can see by the results of the first two tests. However when
given a single parameter it will return another curryfied function taking one 
single parameter as we can see with the last test *)

(* Q4. *)
(* The output of (addc 3) is a function with profile int -> int, we can use this
function to add 3 to any other given number when calling (addc 3). 
On the other hand when calling (addc 3 5) it returns an integer value,similarlly 
((addc 3) 5) will also return an int value, but first, it will compute the 
curryfied function (addc 3) and then apply it to 5 *)

(* Q5. *)
let addC2 (x:int) : int -> int =
  if x = 0 then
    fun y -> y    (* fonction (anonyme) *)
  else
    fun y -> x+y  (* fonction (anonyme) *) 
;;

addC2 3 ;;
(* Output: - : int -> int = <fun> *)
(addC2 30) 12 ;;
(* - : int = 42 *)
addC2 30 12;;
(* - : int = 42 *)
addC2 0 ;;
(* Output: - : int -> int = <fun> *)
addC2 0 42;;
(* - : int = 42 *)

(* All those calls of addC2 works perfectly. In particular, due to variable 
closure in addC2, the function returned in the call (addC2 30) keeps the memory
of the value given and return a function that adds 30 to y *)

 (* 
    Exercice 5.2
    Q1.
*)

(* SPECIFICATION (derivee) gives the value of the derivative of a given function
                 at a given point 
   Profile:      derivee : (float -> float) -> float -> float
   Semantics:    (derive f x) returns the value of the rerivative of f at x 
                 using the formula (f(x+dx)-f(x))/dx with dx = 0.001
   Ex:           (i) (derivee (fun x -> x*x) 2.) = 4                
*)

let derivee (dx: float) (f: float -> float) (x: float) : float =
    (f (x +. dx) -. f (x)) /. dx ;; 

(* Q2. *)

(* We define a function x² *)
let square = fun x -> x*.x ;;
(* We calculate the derivative of x²*)
let square_prime = derivee 0.001 square ;;
(* Output: val square_prime : float -> float = <fun> *)
(* Test of the derivative *)
square_prime 2. ;;
(* Output: - : float = 4.00099999999969924 *)
square_prime 21.2095 ;;
(* Output: - : float = 42.4200000000496402 *)

(* Q3. *)

(* Defining functions *)
let my_sin = fun x -> sin x ;;
let my_cos = fun x -> cos x ;;
let id = fun x -> x ;;
let const_42 = fun (_:float) -> 42. ;;

(* Calculating 1st derivative *)
let my_sin_prime1 = derivee 0.001 my_sin ;;
let my_cos_prime1 = derivee 0.001 my_cos ;;
let id_prime1 = derivee 0.001 id ;;
let const_42_prime1 = derivee 0.001 const_42 ;;

abs_float (my_sin_prime1 0. -. cos 0.) ;;
(* Difference: - : float = 1.6666665836329031e-07 *)
abs_float (my_cos_prime1 0. -. -.sin 0.) ;;
(* Difference: - : float = 0.000499999958325503258 *)
abs_float (id_prime1 0. -. 1.) ;;
(* Difference: - : float = 0. *)
abs_float (const_42_prime1 0. -. 0.) ;;
(* Difference: - : float = 0. *)

(* Calculating 2nd derivative *)
let my_sin_prime2 = derivee 0.001 my_sin_prime1 ;;
let my_cos_prime2 = derivee 0.001 my_cos_prime1 ;;
let id_prime2 = derivee 0.001 id_prime1 ;;
let const_42_prime2 = derivee 0.001 const_42_prime1 ;;

abs_float (my_sin_prime2 0. -. -.sin 0.) ;;
(* Difference: - : float = 0.000999999750117552821 *)
abs_float (my_cos_prime2 0. -. -.cos 0.) ;;
(* Difference: - : float = 5.83282421473541e-07 *)
abs_float (id_prime2 0. -. 0.) ;;
(* Difference: - : float = 0. *)
abs_float (const_42_prime2 0. -. 0.) ;;
(* Difference: - : float = 0. *)

(* Calculating 3th derivative *)
let my_sin_prime3 = derivee 0.001 my_sin_prime2 ;;
let my_cos_prime3 = derivee 0.001 my_cos_prime2 ;;
let id_prime3 = derivee 0.001 id_prime2 ;;
let const_42_prime3 = derivee 0.001 const_42_prime2 ;;

abs_float (my_sin_prime3 0. -. -.cos 0.) ;;
(* Difference: - : float = 1.2504154369707976e-06 *)
abs_float (my_cos_prime3 0. -. sin 0.) ;;
(* Difference: - : float = 0.00150013335087351152 *)
abs_float (id_prime3 0. -. 0.) ;;
(* Difference: - : float = 0. *)
abs_float (const_42_prime3 0. -. 0.) ;;
(* Difference: - : float = 0. *)

(* We can also calculate higher derivatives directly as : *)
let f = fun x -> x*.x*.x ;;
let f_prime3 = derivee 0.001 (derivee 0.001 (derivee 0.001 f)) ;;
abs_float (f_prime3 0. -. 6.) ;;
(* Difference: - : float = 4.44089209850062616e-15 *)

(* Q4. *)

(* a) *)
let rec puiss (n: int) (f: 'a -> 'a) (x: 'a) : 'a =
  match n with
  | i when i > 0 -> puiss (n-1) f (f x)
  | 0 -> x
  | _ -> failwith "Invalid n value" ;;

  (* b) *)
let (>>) (f:'a->'b) (g:'b->'c) : 'a->'c =
  fun x -> g (f x);;

  let rec puiss (n: int) (f: 'a -> 'a) (x: 'a) : 'a =
    match n with
    | i when i > 0-> ((>>) (f) (puiss (n-1) f )) x
    | 0 -> x
    | _ -> failwith "Invalid n value" ;;

(* Q5. *)
(* The 3th derivative of sinus at 0 is : *)
puiss 3 (derivee 0.001) sin 0. ;;

(* Comparing with the result from last question we verify that it is correct *)
assert((abs_float (puiss 3 (derivee 0.001) sin 0. -. -.cos 0.)) = (abs_float (my_sin_prime3 0. -. -.cos 0.)));; 
(* Output: - : unit = () *)

(* Q6. *)

puiss 3 (derivee 0.001 sin) 0. ;;
(* 
   The expression (puiss 3 (derivee 0.001 sin) 0.) is the function 
   (sin' 𝑜 sin' 𝑜 sin')(0) or (cos 𝑜 cos 𝑜 cos)(0). It computes the derivative of 
   the function sin, then composes it 3 times and finally apply it to 0 
*)

(* 
   Exercice 5.3
   Q1.
*)
let inserer (s:'a list) (x:'a) = 
  List.fold_right (fun e acc -> if e<=x then e::acc else acc) s [] @
  x :: List.fold_right (fun e acc -> if e>x then e::acc else acc) s [];;

(* Function test *)
assert((inserer [1;3] 0) = [0;1;3]);;
(* Output: - : unit = () *)
assert((inserer [1;3] 1) = [1;1;3]);;
(* Output: - : unit = () *)
assert((inserer [1;3] 2) = [1;2;3]);;
(* Output: - : unit = () *)
assert((inserer [1;3] 3) = [1;3;3]);;
(* Output: - : unit = () *)
assert((inserer [1;3] 4) = [1;3;4]);;
(* Output: - : unit = () *)

(* Q2. *)
let insertion_sort (s:'a list) : 'a list =
  List.fold_right (fun e acc -> inserer acc e) s [];;

(* Function test *)
assert((insertion_sort [10;4;100;2;5;3;4242;42;1]) = [1;2;3;4;5;10;42;100;4242]);;
(* Output: - : unit = () *)

(* 
   Exercice 5.4.1 
   Q1.
*)
let suffixes (s: 'a list) : 'a list list =
	List.rev (List.fold_right (fun e acc -> 
				match acc with
				| [] | [[]] -> [[e]; []]
				| pr::_ -> (e::pr)::acc
		) s [[]]) ;;

(* Function test *)
assert((suffixes [2;3;1]) = [[];[1];[3;1];[2;3;1]]);;


(* Q2. *)

(* La sortie de prefixes de [2;3;1] devrait etre [[]; [2]; [2; 3]; [2; 3; 1]] et 
la sortie de suffixes de [1;3;2] est [[]; [2]; [3; 2]; [1; 3; 2]]. On voit bien
que les elements de chaque liste dans le resultat sont les memes, mais ils sont 
renversés en ordre. *)

(* Q3. *)
let prefixes (s: 'a list) : 'a list list =
	List.rev (List.fold_left (fun acc e -> 
				match acc with
				| [] | [[]] -> [[e]; []]
				| pr::_ -> (pr@[e])::acc
		) [[]] s);;

(* Function test *)
assert((prefixes [2;3;1]) = [[];[2];[2;3];[2;3;1]]);;

(* Q4. *)

(* To pass from prefixes of [e0;e1] to prefixes of [e;e1;e2] we need to 
concatenate the element e2 to the end of the sequence [e0;e1] with the operation
" [e0;e1]@[e2]". For that we create a list containing e2 and then we concatenate
it at the endo of the list [e0;e1] *)

(* Q5. *)

(* Same question as Q3. See Q3. *)

let ajoutAchq (x:'a) (s:'a list list) : 'a list list = 
  List.fold_right (fun e acc -> (x::e)::acc) s [];;

(* Function test *)
assert((ajoutAchq 0 [[2; 3];[5];[];[3; 8]]) = [[0;2;3];[0;5];[0];[0;3;8]]);;

(* Q7. *)
let prefixes (s:'a list) : 'a list list =
  List.fold_right (fun e acc -> []::(ajoutAchq e acc)  ) s [[]] ;;

(* Function test *)
assert((prefixes [2;3;1]) = [[];[2];[2;3];[2;3;1]]);;