(* 
   EXERCICE 2.1.3 
   Q11.
*)

assert ((5=3) = false);;        (* This expression will output nothing (unit) \
 - : unit = ()                     So the expression is correct *)

assert ((5=5) = true);;         (* This expression will output nothing (unit) \
 - : unit  = ()                    So the expression is correct *)

assert ((5=3) = true);;         (* This expression will raise an exception as 5 <> 3 \
Exception: Assert_failure ("//toplevel//", 1, 0). *)

(* 
   One can conclude that the expression "assert " will raise an error when the boolean \
value returned by a given expression is false.
*)

(* 
   Q 12. As expected for the third expression, an assertion is raised
*)

(*
   EXERCICE 2.1.4
   Q31.
*)

let a:int = 8 ;;        (* Declaring int a *)

a ;;                    (* This will output the type and value of the variable a 
 - : int = 8               As expected it prints the type and value of a*)

a + 5 ;;                (* It will display the value and type of the expression 8 + 5
 - : int = 13              It prints 13 as expected *)

a +. 9.1 ;;             (* This operation will raise an error as it is a float operation \
                           and a is of type int 
Error:This expression has type int but an expression was expected of type float *)

