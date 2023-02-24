(**
  Already use TRMC implementations for various list operations that are not yet
  relased.
  
  Source: https://github.com/ocaml/ocaml/blob/trunk/stdlib/stdlib.ml
  *)

let[@tail_mod_cons] rec ( @ ) l1 l2 =
  match l1 with
  | [] -> l2
  | h1 :: [] -> h1 :: l2
  | h1 :: h2 :: [] -> h1 :: h2 :: l2
  | h1 :: h2 :: h3 :: tl -> h1 :: h2 :: h3 :: (tl @ l2)

