let packageBoxing package boxes =
    let vol x = List.fold ( * ) 1 x in
    let pack = List.sort package in
    let smaller p b = vol p < vol b in
    let fits b = List.zip pack b |>
        List.forall (fun (x,y) -> x <= y)
    in
    let fold_f (r, i, ob) b =
        let box = List.sort b in
        let isSmaller = ob
            |> Option.map (smaller box)
            |> Option.getOrElse true
        in
        if fits box && isSmaller
        then (i, i+1, Some box)
        else (r, i+1, ob)
    in
    let (r,_,_) = List.fold fold_f ((-1), 0, None) boxes in
    r
