(*Exercice 2.9*)
(*Q1/*)
(*Creating the function div as it is defined with the profil and semantic already noted in the poly*)
let div (n:int) (d:int)=
  (n/d,n mod d);; 
(*The function takes 2 integers and returns the couple of the quotient and the rest that we can directly 
write like a couple with the necessary functions we will just have to name each element to use them with let (q,r)=f(...) in ...*)

(*Q2/*)
(*Creating the function div as it is defined with the profil and semantic already noted in the poly and using div*)
(*/!\ Use condition: n as to be between 0 and 9999*)
let sc (n:int):int=
  let (diz,u)=div n 10 in 
    let (cent,d)=div diz 10 in
      let (mil,c)=div cent 10 in
        let m=mil mod 10 in
          m+c+d+u;;
(*This function uses the previous number that we calculate everytime to calculate the next one.
It first does the euclidian division and the rest for the number of unit and then
it calculates the euclidian division of the number of 10's by using the previous division 
and it does the same for 100's. For the number of 1000's it does the same except that
it doesn't calculate the divison because we would have no need for after and we only need the modulo.
Then all of the number of thousands, hundreds,etc... are summed up which gives us the sum of the number
*)
(*Fondamental tests*)
sc 0;; (*Returns 0 as expected and it is the lowest we can have because of our condition*)
sc 9999;; (*Returns 36 as expected and it is the biggest number we can have because of our condition*)
sc 6234;; (*Returns 15 as expected and it is just a random test number*)
(*Q3/*)
#trace div;;
#trace sc;;
sc 2345;;
(*Here is the indent version of the trace
sc <-- 2345
div <-- 2345
div --> <fun>
div* <-- 10
div* --> (234, 5)
    div <-- 234
    div --> <fun>
    div* <-- 10
    div* --> (23, 4)
      div <-- 23
      div --> <fun>
      div* <-- 10
      div* --> (2, 3)
sc --> 14
We can see that it correspond to what we said earlier*)
#untrace div;;
#untrace sc;;
