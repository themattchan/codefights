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

let segment x y =
  if abs x > abs y then 0
  else if x > 0 && y > 0 && abs x = abs y then 1
  else if x < 0 && y > 0 && abs x = abs y then 2
  else (*if x < 0 && y < 0 && abs x = abs y then*) 3


let turnsOnRoad x y =
  if (x,y) = (0,0) then 0 else
  let i = max (abs x - 1) 0 in
  (4 * i) + (segment x y)

How to find closest turn point
Test 2: (1,-1) is between (-1,-1) and (2,-1)
Test 3: (0,1) is between (1,1) and (-1,1)
Test 10: (3,5) is between ...
*)

exception Fail
let segment x y =
  if abs x > abs y then 0
  else if x > 0 && y > 0 && abs x = abs y then 1
  else if x < 0 && y > 0 && abs x = abs y then 2
  else (*if x < 0 && y < 0 && abs x = abs y then*) 3

let isCornerPoint x y =
   x > y && x+y = 1 ||
   x > 0 && y > 0 && x = y ||
   x < 0 && y > 0 && abs x = y ||
   x < 0 && y < 0 && x = y

let isOnVertical x y =
  if x > 0 then y >= (1-x) && y <= x
    else y >= -x && y <= x

let isOnHorizontal x y =
  if y < 0 then x >= y && x <= ((abs y) + 1 )
    else x >= -y && x <= y

type point = int * int
// don't need so much information
type bound = Corner of point | Line of point

(*
let boundingPair x y =
  if isCornerPoint x y then ((x,y), (x,y))
  else if isOnVertical x y then
    if x > 0 then ((x,1-x), (x, x))
      else ((x,x), (x,-x))
  else if isOnHorizontal x y then
    if y < 0 then ((y,y), (abs y +1, y))
      else ((y,y), (-y,y))
  else raise Fail
*)

let boundingPair x y =
  if isCornerPoint x y then Corner (x,y)
  else if isOnVertical x y then
    if x > 0 then Line (x,1-x)
      else Line (x,x)
  else if isOnHorizontal x y then
    if y < 0 then Line (y,y)
      else Line (y,y)
  else raise Fail

let turns x y =
  let i = max (abs x - 1) 0 in
    (4 * i) + (segment x y)

let turnsOnRoad x y =
  if (x,y) = (0,0) then 0 else
  match boundingPair x y with
    | Corner (x,y) -> turns x y
    | Line   (x,y) -> turns x y + 1
