open Logtk
open Libzipperposition

module T = Term
module S = Subst
module US = Unif_subst

open TestTerm

module Preunif = Constraints.Make(struct
  let st = Flex_state.empty
          |> Flex_state.add PragUnifParams.k_max_inferences (-1)
          |> Flex_state.add PragUnifParams.k_try_lfho true
end)

let test_trivial_success = "test_trivial_success", `Quick, fun () ->
  let seq = Preunif.unify_scoped (a,0) (a,1) in
  let l = OSeq.to_list seq |> List.filter_map Fun.id in
  Alcotest.(check int) "len" (List.length l) 1;
  let unif = List.hd l in
  Alcotest.(check bool) "empty subst" (US.is_empty unif) true

let suite_preunif : unit Alcotest.test_case list = [
  test_trivial_success;
]