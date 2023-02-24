(**
  Already use TRMC implementations for various list operations that are not yet
  relased.
  
  
  *)

(** TMRC list append 
    
    Source: https://github.com/ocaml/ocaml/blob/trunk/stdlib/stdlib.ml
*)
let[@tail_mod_cons] rec ( @ ) l1 l2 =
  match l1 with
  | [] -> l2
  | h1 :: [] -> h1 :: l2
  | h1 :: h2 :: [] -> h1 :: h2 :: l2
  | h1 :: h2 :: h3 :: tl -> h1 :: h2 :: h3 :: (tl @ l2)


(** TMRC list functions 
    
    Source: https://github.com/ocaml/ocaml/blob/trunk/stdlib/list.ml
*) 
module FList = struct
  let[@tail_mod_cons] rec map f = function
    [] -> []
  | [a1] ->
      let r1 = f a1 in
      [r1]
  | a1::a2::l ->
      let r1 = f a1 in
      let r2 = f a2 in
      r1::r2::map f l

  let[@tail_mod_cons] rec mapi i f = function
      [] -> []
    | [a1] ->
        let r1 = f i a1 in
        [r1]
    | a1::a2::l ->
        let r1 = f i a1 in
        let r2 = f (i+1) a2 in
        r1::r2::mapi (i+2) f l
  
  let mapi f l = mapi 0 f l

  let[@tail_mod_cons] rec find_all p = function
    | [] -> []
    | x :: l -> if p x then x :: find_all p l else find_all p l


  let filter = find_all 
  
  let[@tail_mod_cons] rec filter_map f = function
    | [] -> []
    | x :: l ->
        match f x with
        | None -> filter_map f l
        | Some v -> v :: filter_map f l

  let[@tail_mod_cons] rec concat_map f = function
    | [] -> []
    | x::xs -> prepend_concat_map (f x) f xs
  and[@tail_mod_cons] prepend_concat_map ys f xs =
    match ys with
    | [] -> concat_map f xs
    | y :: ys -> y :: prepend_concat_map ys f xs

  let[@tail_mod_cons] rec map2 f l1 l2 =
    match (l1, l2) with
      ([], []) -> []
    | ([a1], [b1]) ->
        let r1 = f a1 b1 in
        [r1]
    | (a1::a2::l1, b1::b2::l2) ->
        let r1 = f a1 b1 in
        let r2 = f a2 b2 in
        r1::r2::map2 f l1 l2
    | (_, _) -> invalid_arg "List.map2"
end