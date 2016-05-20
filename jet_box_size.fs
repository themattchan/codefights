let packageBoxing package boxes =
  let vol x = List.fold ( * ) 1 x in
  let pack = List.sort package in
  let smaller p b = vol p < vol b in
  let fits b = List.forall (fun (x,y) -> x <= y) (List.zip pack b)
//  let size_tf = size package in
  let fold_f (r,i, ob) b =
    let box = List.sort b
    match ob with
    | None -> if fits box then (i, i+1, Some box) else (r,i+1, ob)
    | Some oldbox -> if fits box && smaller box oldbox then (i, i+1, Some box) else (r, i+1,ob)
  in
  let (r,_,_) = List.fold fold_f ((-1), 0, None) boxes in r
