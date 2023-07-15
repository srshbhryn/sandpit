
type bst = Empty | Node of int * bst * bst

let rec search n = function
  | Empty -> false
  | Node (v, l, r) -> v = n || if v > n then search n l else search n r

let rec add n = function
  | Empty -> Node (n, Empty, Empty)
  | Node (v, l, r) -> if v >= n then Node (v, add n l, r)  else Node (v, l, add n r)

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

)
