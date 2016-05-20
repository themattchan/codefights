module jet
open System

let rec go_insert things more =
    match (things,more) with
    | ([], rest) -> rest
    | things, [] -> things
    | (t::ts), (m::ms) -> if t > m then (m :: t :: go_insert ts ms)
                              else (t :: go_insert ts more)

let rec go catalog updates newcat =
    match (catalog,updates) with
    | [],_ -> List.rev newcat
    | log,[] -> List.rev (log@newcat)
    | ((k1::v1)::logs), ((k2::v2)::ups) ->
        if k1 = k2 then go logs ups ((k2::(go_insert v1 v2)) :: newcat)
            else if k1 < k2 then go logs updates ((k1::v1) :: newcat)
            else (* k1 > k2 *)   go catalog ups ((k2::v2) :: newcat)

let catalogUpdate catalog updates =
    let updates' = List.sortBy List.head updates in
    go catalog updates' []

let catalog1 = [["Books";"Classics";"Fiction"];
                ["Electronics";"Cell Phones";"Computers";"Ultimate item"];
                ["Grocery";"Beverages";"Snacks"];
                ["Snacks";"Chocolate";"Sweets"];
                ["root";"Books";"Electronics";"Grocery"]]

let updates1 =  [["Snacks";"Marmalade"];
                 ["Fiction";"Harry Potter"];
                 ["root";"T-shirts"];
                 ["T-shirts";"CodeFights"]]

let expect1 = [["Books";"Classics";"Fiction"];
               ["Electronics";"Cell Phones";"Computers";"Ultimate item"];
               ["Fiction";"Harry Potter"];
               ["Grocery";"Beverages";"Snacks"];
               ["Snacks";"Chocolate";"Marmalade";"Sweets"];
               ["T-shirts";"CodeFights"];
               ["root";"Books";"Electronics";"Grocery";"T-shirts"]]

let _ = catalogUpdate catalog1 updates1 = expect1
