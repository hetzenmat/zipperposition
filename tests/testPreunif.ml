open Logtk
open Libzipperposition

module T = Term
module S = Subst
module US = Unif_subst

open TestTerm

let a_ = ID.make "a"
let b_ = ID.make "b"
let f_ = ID.make "f"

let ty = Type.term
let ty' = Type.([ty] ==> ty)


let a = T.const ~ty a_
let b = T.const ~ty b_

let f_fun = T.const ~ty:ty' f_
let f t = T.app f_fun [t]

let _F = T.var_of_int ~ty:ty' 0
let _G = T.var_of_int ~ty:ty' 1

let cstr_test = Alcotest.testable Unif_constr.pp Unif_constr.equal

module Preunif = Constraints.Make(struct
  let st = Flex_state.empty
          |> Flex_state.add PragUnifParams.k_max_inferences (-1)
          |> Flex_state.add PragUnifParams.k_try_lfho false
          |> Flex_state.add PragUnifParams.k_sort_constraints false
end)

let test_trivial_success = "test_trivial_success", `Quick, fun () ->
  let seq = Preunif.unify_scoped (a,0) (a,1) in
  let l = OSeq.to_list seq |> List.filter_map Fun.id in
  Alcotest.(check int) "len" (List.length l) 1;
  let unif = List.hd l in
  Alcotest.(check bool) "empty subst" (US.is_empty unif) true

let test_1 = "test_1", `Quick, fun () ->
  let lhs = T.app _F [(T.app _G [a])] in (* F (G a) *)
  let rhs = T.app _F [b] in (* F b *)
  
  let seq = Preunif.unify_scoped (lhs,0) (rhs,0) in
  let l = OSeq.to_list seq |> List.filter_map Fun.id in
  Alcotest.(check int) "len" (List.length l) 1;
  let us = List.hd l in
  let subst, constrl_l = US.subst us, US.constr_l us in
  US.to_string us |> print_endline;
  
  Alcotest.(check bool) "is renaming" true (Subst.is_renaming subst);
  Alcotest.(check int) "constraint size" 1 (List.length constrl_l);

  let constr = List.hd constrl_l in

  Alcotest.(check int) "new scopes" (Unif_constr.sc1 constr) (Unif_constr.sc2 constr);

  let sc = Unif_constr.sc1 constr in

  Alcotest.(check cstr_test) "constraints" (List.hd constrl_l) (Unif_constr.make ~tags:[] ((lhs : T.t :> InnerTerm.t),sc) ((rhs : T.t :> InnerTerm.t),sc))

let test_2 = "test_2", `Quick, fun () ->
  let lhs' = T.app _G [a] in 
  let rhs' = T.app _F [b] in
  let lhs = CCSeq.iterate f lhs' |> CCSeq.drop 100 |> CCSeq.head_exn in
  let rhs = CCSeq.iterate f rhs' |> CCSeq.drop 100 |> CCSeq.head_exn in

  let seq = Preunif.unify_scoped (lhs,0) (rhs,0) in
  let l = OSeq.to_list seq |> List.filter_map Fun.id in
  Alcotest.(check int) "len" (List.length l) 1;
  let us = List.hd l in
  let subst, constrl_l = US.subst us, US.constr_l us in
  US.to_string us |> print_endline;
  
  Alcotest.(check bool) "is renaming" true (Subst.is_renaming subst);
  Alcotest.(check int) "constraint size" 1 (List.length constrl_l);

  let constr = List.hd constrl_l in

  Alcotest.(check int) "new scopes" (Unif_constr.sc1 constr) (Unif_constr.sc2 constr);

  let sc = Unif_constr.sc1 constr in

  Alcotest.(check cstr_test) "constraints" (List.hd constrl_l) (Unif_constr.make ~tags:[] (((T.app (T.var_of_int ~ty:ty' 2) [a]) : T.t :> InnerTerm.t),sc) (((T.app (T.var_of_int ~ty:ty' 3) [b]) : T.t :> InnerTerm.t),sc))

let suite_preunif : unit Alcotest.test_case list = [
  test_trivial_success;
  test_1;
  test_2;
]