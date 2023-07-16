
type bst = Empty | Node of int * bst * bst

let rec search n = function
  | Empty -> false
  | Node (v, l, r) -> v = n || if v > n then search n l else search n r

let rec add n = function
  | Empty -> Node (n, Empty, Empty)
  | Node (v, l, r) -> if v >= n then Node (v, add n l, r)  else Node (v, l, add n r)

let rec remove_and_return_least_element = function
  | Empty -> (Empty, 0)
  | Node (v, r, Empty) -> (r, v)
  | Node (v, r, l) -> let rr, m = remove_and_return_least_element l in Node (v, r, rr), m

let rec delete n = function
  | Empty -> Empty
  | Node (v, l, r) ->
    if v > n then
      Node (v, delete n l, r)
    else if v < n then
      Node (v, l, delete n r)
    else (* n = v *)
      match l, r with
      | Empty, r -> r
      | l, Empty -> l
      | l, r -> let rr, m = remove_and_return_least_element r in Node (m, l, rr)

let t = add 3 (add 2 (add 1 Empty))

let () = (
  if search 12 t then
    print_endline "12"
  else
    print_endline "no 12"
  ;

  if search 1 t then
    print_endline "1"
  else
    print_endline "no 1"
  ;
)
