(* a. Convert the sumNT function into a tail recursive version using
two different functions called sum_helper and sum_TR. *)

let rec sumNT i stop =
  if i = stop
  then stop
  else i+(sumNT (i+1) stop)

let rec sum_helper i basei acc stop = 
  if (stop - basei + 1) == acc
  then i
  else sum_helper (i+(basei+acc)) (basei) (acc + 1) stop

let rec sumTR i stop = sum_helper i i 1 stop;;

let a = 1;;
let b = 2;;

sumNT a b;;
sumTR a b;;


(* b. Find inputs where sum_NT issues a stack overflow, but sum_TR
   computes a value.  Define blowsStackInput to be the pair of the
   inputs you found that blows the stack, then define computesValue
   as the application of sum_TR to blowsStackInput () using the
   functions:

First: fst : a' * b' -> a'
Second: snd : a' * b' -> b' *)

let blowsStack () = (1, 100000)
  
let computesValue () = let (m,n) = blowsStack() in sumTR m n


(* c. Evaluate sum_TR 1 2 using the call stack and activation frames.
Write out each activation frame as you progress just as we did in the
videos. Place your solution in the designated comment. )

(
  init: main = <fun>, sumTR = <fun>
     sum_helper: i = 3, basei = 1, acc = 2, stop = 2
)

( d. Do an online search for "compiler optimizations" and read about one
we did not cover in class. Summarize in a comment the optimization you
found.  Make sure to include the link to where you read about it. )

(
  When it was very difficult to divide by an integer, compilers would instead
   divide using bit operations and multiplication.
   https://queue.acm.org/detail.cfm?id=3372264
*)