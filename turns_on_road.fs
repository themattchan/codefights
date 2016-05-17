(* Sequence is
0,0
spiral num:
1:
  1,0  t = 0
  1,1  t = 1
 -1,1  t = 2
 -1,-1 t = 3
2:
  2,-1 t = 4
  2,2  t = 5
 -2,2  t = 6
 -2,-2 t = 7
3:
  3,-2 t = 8
  3,3  t = 9
 -3,3  t = 10
 -3,-3 t = 11
4:
...

Sequence is:

f (x,y) n = [(n,y), (n,n), (-n,n), (-n,-n)]
where y = -(n-1)

for init (x,y) = (0,0) and n = [1..]

How to calculate the spiral num from a coord:

num_of_coord (x,y) = |x|

The # segment of a coord:

map into the sequence

segment (x,y) =
  if |x| > |y| then 0
  else if x > 0 && y > 0 && |x| = |y| then
  else if x < 0 && y > 0 && |x| = |y| then 3
  else if x < 0 && y < 0 && |x| = |y| then 4

The nth turn is:

nth (x,y) = 4 * (|x|-1) + segment (x,y)

*)

(*
This is only valid if the inputs are the exact turn points

let segment x y : int =
  if abs x > abs y then 0
  else if x > 0 && y > 0 && abs x = abs y then 1
  else if x < 0 && y > 0 && abs x = abs y then 2
  else (*if x < 0 && y < 0 && abs x = abs y then*) 3


let turnsOnRoad x y =
  let i = max (abs x - 1) 0 in
  (4 * i) + (segment x y)


How to find closest turn point

*)
