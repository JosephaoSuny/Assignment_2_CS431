let rec replicate x n = match n with 
  | 0 -> []
  | v -> if v > 0 then [x] @ replicate x (v-1) 
         else raise (Failure "Cannot have negative sized list")
;;

let replicate_tail x n = 
  let rec helper x n acc = 
    if n = 0 then acc 
    else if n > 0 then helper x (n-1) acc 
    else raise (Failure "Cannot have negative sized list") 
  in helper x n []
;;

let rec makeList n = 
  if n = 0 then []
  else if n > 0 then (makeList (n-1) @ [n])
  else raise (Failure "Cannot have negative sized list")
;;

let a = makeList 4;;

print_int (List.nth a 3);;
print_endline;;

let makeListTail n = 
  let rec helper n acc =
    if n = 0 then acc
    else if n > 0 then helper (n-1) ([n] @ acc)
    else raise (Failure "Cannot have negative sized list")
  in helper n []
;;

let rec reverse a = match a with | [] -> [] | hd::tl -> reverse tl @ [hd];;

let reverseTail a = let rec helper a acc = match a with | [] -> acc | hd::tl -> hd @ helper tl acc in helper a [];;

type 'a btree = Empty | Leaf of 'a | Node of ('a * 'a btree * 'a btree);;

let tst = Node(5, Leaf 2, Node(12, Node(8, Leaf 7, Leaf 10), Leaf 49));;

let rec countLeaves t = match t with | Empty -> 0 | Leaf _ -> 1 | Node(_, t1, t2) -> 1 + countLeaves t1 + countLeaves t2;;

let rec toListOrdered t = match t with | Empty -> [] | Leaf v -> [v] | Node(v, t1, t2) -> toListOrdered t1 @ [v] @ toListOrdered t2;;

let rec toListPreOrder t = match t with | Empty -> [] | Leaf v -> [v] | Node(v, t1, t2) -> [v] @ toListPreOrder t1 @ toListPreOrder t2;;

let () = print_endline "Hello, World!"
