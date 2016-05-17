(*
 a website has n pages. for any two pages there exists a link.

How many links?

n choose 2, i.e. n!/[2! (n-2)!]

Do we include self references in the link breaking?

for each link, 0.5 chance that it is broken

then P(none broken) = 0.5 ^ #links

*)

let ncr n k =
  let rec aux a n k =
    if k = 0 then a
    else aux ((n * a)/k) (n-1) (k-1)
  in go 1 n k

(*
(1/2^k)

10^15 = 2^x

where x = 15(log2+log5)/log2

so ==> 2^(x-k)
*)
(*
let exp_x =
  let log x = Math.Log((float x), 10.) in
  (15. * (log 2 + log 5)) / (log 2)

let Hacked_Website n =
  2 ** (exp_x - (float (ncr n 2))) |> sprintf "%f"
*)

let big_num = pown 10 15

let Hacked_Website n =
  big_num / (ncr n 2) |> sprintf "%d"
