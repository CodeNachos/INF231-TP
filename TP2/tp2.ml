(*EXERCICE 2.4*)

(*Q1/*)
(*We define the variables*)
let a:int = 10;; 
let b:int = 0 ;;
(b <> 0) && (a mod b=0);; (*This gives a boolean which is false because && checks the boolean on the left first and if it's false then it return false because of the table of truth*)
(* (a mod b=0) && (b <> 0);; *) (*This gives an error because it first check the boolean on the left but since b=0 a mod b can't exists since it's a division by 0 so it returns an error*)
(*The rule for && is that it checks the boolean on the left, if it's true it compares it to the one on the right, if it's false it returns false without even checking the right one and if it's an error it show the error without checking the right one*)

(*Q2/*)
(*We define the function monExpression*)
let monExpression (a:int)(b:int):bool= 
  (b<>0) && (a mod b=0);; (*This evaluate the two booleans and compares them to be able to see if it's true or false in the table of truth*)
monExpression 10 0 ;;(*Evaluates the function monExpression with the parameters being a=10 and b=0*)
(*We can see that it does the same thing as before because in the function it checks if b is 0 first*)

(*Q3/*)
(*We define the function monEt*)
let monEt (x:bool) (y:bool):bool=
  x && y;; (*Evaluate x and y and returns then the boolean based on the table of truth*)
monEt true false ;; (*This returns false because it checks first the left object which is true so it checks the one on the right but it's false so by the table it's false*)
monEt false true ;; (*This returns false because it checks first the left object which is false so it just returns false as said earlier*)
(* monEt (b <> 0) (a mod b = 0) ;; *) (*This raises an error of type : Exception : Division_by_zero that we are going to explain just below int the conclusion*)
(*Conclusion*)
(*We can conclude that when we call a function, it first evaluates the parameters that we putted before entering the function so if there is an error in one of the paramater
  ,which is our case with monEt (b <> 0) (a mod b=0) because a mod b raises an error, OCaml will just say there is an error and won't execute the function *)
