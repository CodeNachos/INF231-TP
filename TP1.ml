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

