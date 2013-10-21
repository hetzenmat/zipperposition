
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {6 Arithmetic Manipulations} *)

open Logtk

module S = Symbol
module Literals = Literal.Arr

let prof_arith_simplify = Util.mk_profiler "arith.simplify"
let prof_arith_extract = Util.mk_profiler "arith.extract"

(** {2 Utils} *)

let is_arith_ty ty =
  Type.eq ty Type.int || Type.eq ty Type.rat || Type.eq ty Type.real

(** {2 Terms} *)

module T = struct
  include FOTerm  (* for the rest of arith *)

  let rec is_arith t = match t.term with
    | Node ((S.Int _ | S.Rat _ | S.Real _), []) -> true
    | Node (s, _) when Symbol.Arith.is_arith s -> true
    | Var _
    | BoundVar _ ->
      begin match t.type_ with
      | Some ty -> is_arith_ty ty
      | None -> assert false
      end
    | _ -> false

  let is_arith_const t = match t.term with
    | Node ((S.Int _ | S.Rat _ | S.Real _), []) -> true
    | _ -> false

  let rec sum_list l = match l with
    | [] -> failwith "Arith.sum_list: got empty list"
    | [x] -> x
    | x::l' -> mk_node S.Arith.sum [x; sum_list l']

  let mk_sum t1 t2 = mk_node S.Arith.sum [t1; t2]
  let mk_difference t1 t2 = mk_node S.Arith.difference [t1; t2]
  let mk_product t1 t2 = mk_node S.Arith.product [t1; t2]
  let mk_quotient t1 t2 = mk_node S.Arith.quotient [t1; t2]
  let mk_uminus t = mk_node S.Arith.uminus [t]
  
  let mk_less t1 t2 = mk_node S.Arith.less [t1; t2]
  let mk_lesseq t1 t2 = mk_node S.Arith.lesseq [t1; t2]

  let extract_subterms t =
    (* recursive function that gathers terms into set *)
    let rec gather set t = match t.term with
    | Var _
    | BoundVar _ -> Tbl.replace set t ()
    | Node (s, []) when S.is_numeric s -> ()
    | Node (s, l) when S.Arith.is_arith s ->
      List.iter (gather set) l
    | Node _ -> Tbl.replace set t ()
    in
    if is_arith t
      then
        let set = Tbl.create 5 in
        let () = gather set t in
        Tbl.to_list set
      else []

  let _var_occur_strict v t =
    not (eq v t) && var_occurs v t

  let shielded v t = match extract_subterms t with
    | [] -> _var_occur_strict v t
    | l -> List.exists (fun t' -> _var_occur_strict v t') l

  (* TODO: a flag for simplified terms, makes simplifying them faster *)

  let simplify ~signature t =
    Util.enter_prof prof_arith_simplify;
    (* recursive function with cache *)
    let rec simplify recurse t = match t.term with
    | Var _
    | BoundVar _ -> t
    | Node (s, [t']) ->
      let t' = recurse t' in
      try_reduce_unary recurse s t'
    | Node (s, [t1; t2]) ->
      let t1 = recurse t1 in
      let t2 = recurse t2 in
      try_reduce_binary recurse s t1 t2
    | Node (s, l) ->
      let t = mk_node s (List.map recurse l) in
      t
    (** unary builtins *)
    and try_reduce_unary recurse s a =
      match s, a.term with
      | S.Const ("$uminus", _), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.uminus n)
      | S.Const ("$uminus",_), Node (S.Const ("$uminus",_), [x]) -> x
      | S.Const ("$floor",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.floor n)
      | S.Const ("$ceiling",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.ceiling n)
      | S.Const ("$round",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.round n)
      | S.Const ("$truncate",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.truncate n)
      | S.Const ("$is_int",_), Node (n, []) when S.is_numeric n ->
        if S.is_int n then true_term else false_term
      | S.Const ("$is_rat",_), Node (n, []) when S.is_numeric n ->
        if S.is_rat n then true_term else false_term
      | S.Const ("$is_real",_), Node (n, []) when S.is_numeric n ->
        if S.is_real n then true_term else false_term
      | S.Const ("$to_rat",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.to_rat n)
      | S.Const ("$to_real",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.to_real n)
      | S.Const ("$to_int",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.to_int n)
      | _ -> mk_node s [a]  (* default case *)
    (** binary builtins *)
    and try_reduce_binary recurse s a b =
      try begin match s, a.term, b.term with
      | S.Const ("$sum",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.sum na nb)
      | S.Const ("$difference",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.difference na nb)
      | S.Const ("$product",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.product na nb)
      | S.Const ("$quotient",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        begin try mk_const (S.Arith.Op.quotient na nb)
        with S.Arith.TypeMismatch _ -> mk_quotient a b
        end
      | S.Const ("$quotient_e",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.quotient_e na nb)
      | S.Const ("$quotient_t",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.quotient_t na nb)
      | S.Const ("$quotient_f",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.quotient_f na nb)
      | S.Const ("$remainder_e",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.remainder_e na nb)
      | S.Const ("$remainder_t",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.remainder_t na nb)
      | S.Const ("$remainder_f",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.remainder_f na nb)
      | S.Const ("$less",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        if S.Arith.Op.less na nb then true_term else false_term
      | S.Const ("$lesseq",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        if S.Arith.Op.lesseq na nb then true_term else false_term
      | S.Const ("$greater",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        if S.Arith.Op.greater na nb then true_term else false_term
      | S.Const ("$greatereq",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        if S.Arith.Op.greatereq na nb then true_term else false_term
      | S.Const ("$sum",_), _, Node (nb,[]) when S.Arith.is_zero nb -> recurse a
      | S.Const ("$sum",_), Node (na,[]), _ when S.Arith.is_zero na -> recurse b
      | S.Const ("$difference",_), _, Node (nb,[]) when S.Arith.is_zero nb -> recurse a
      | S.Const ("$difference",_), Node (na,[]), _ when S.Arith.is_zero na ->
        recurse (mk_uminus b)
      | S.Const ("$difference",_), _, _ when eq a b ->
        (* need to infer types so  that we know which zero to return *)
        let ty = TypeInference.FO.infer_sig signature a in
        mk_const (S.Arith.zero_of_ty ty)
      | S.Const ("$product",_), _, Node (nb,[]) when S.Arith.is_one nb -> a
      | S.Const ("$product",_), Node (na,[]), _ when S.Arith.is_one na -> b
      | S.Const ("$product",_), Node (na,[]), _ when S.Arith.is_zero na -> a
      | S.Const ("$product",_), _, Node (nb,[]) when S.Arith.is_zero nb -> b
      | S.Const ("$quotient",_), _, Node (nb,[]) when S.Arith.is_one nb -> a
      | S.Const ("$quotient",_), Node (na,[]), _ when S.Arith.is_zero na -> a
      | _ -> mk_node s [a; b]  (* default case *)
      end with Division_by_zero ->
        mk_node s [a; b]
    in
    let __cache = TCache.create 9 in
    let t' = TCache.with_cache_rec __cache simplify t in
    Util.exit_prof prof_arith_simplify;
    t'
end

(** {2 Formulas} *)

module F = struct
  include FOFormula

  let rec simplify ~signature f = match f.form with
  | True
  | False -> f
  | Not {form=Atom {T.term=T.Node(S.Const("$greater",_), [l;r])}} ->
    simplify ~signature (mk_atom (T.mk_lesseq l r))
  | Not {form=Atom {T.term=T.Node(S.Const("$greatereq",_), [l;r])}} ->
    simplify ~signature (mk_atom (T.mk_less l r))
  | Not {form=Atom {T.term=T.Node(S.Const("$less",_), [l;r])}} ->
    simplify ~signature (mk_atom (T.mk_lesseq r l))
  | Not {form=Atom {T.term=T.Node(S.Const("$lesseq",_), [l;r])}} ->
    simplify ~signature (mk_atom (T.mk_less r l))
  | Atom {T.term=T.Node(S.Const("$greater",_), [l;r])} ->
    simplify ~signature (mk_atom (T.mk_less r l))
  | Atom {T.term=T.Node(S.Const("$greatereq",_), [l;r])} ->
    simplify ~signature (mk_atom (T.mk_lesseq r l))
  | Or l -> mk_or (List.map (simplify ~signature) l)
  | And l -> mk_and (List.map (simplify ~signature) l)
  | Not {form=Equal(l,r)} ->
    let l' = T.simplify ~signature l in
    let r' = T.simplify ~signature r in
    if T.eq l' r'
      then mk_false
      else mk_neq l' r'
  | Not f' -> mk_not (simplify ~signature f')
  | Equiv (f1, f2) -> mk_equiv (simplify ~signature f1) (simplify ~signature f2)
  | Imply (f1, f2) -> mk_imply (simplify ~signature f1) (simplify ~signature f2)
  | Atom p ->
    let p' = T.simplify ~signature p in
    mk_atom p'
  | Equal (l, r) ->
    let l' = T.simplify ~signature l in
    let r' = T.simplify ~signature r in
    if T.is_arith_const l'  && T.is_arith_const r' && l' != r'
      then mk_false
      else mk_eq l' r'
  | Forall f' -> mk_forall (simplify ~signature f')
  | Exists f' -> mk_exists (simplify ~signature f')
end

(** {2 View a Literal as an arithmetic Literal}. *)

module Lit = struct
  type t =
  | True   (* arithmetic tautology *)
  | False  (* arithmetic absurdity *)
  | Eq of FOTerm.t * Monome.t
  | Neq of FOTerm.t * Monome.t
  | L_less of FOTerm.t * Monome.t   (* term < monome *)
  | L_lesseq of FOTerm.t * Monome.t
  | R_less of Monome.t * FOTerm.t
  | R_lesseq of Monome.t * FOTerm.t

  let pp buf lit = match lit with
  | True -> Buffer.add_string buf "true"
  | False -> Buffer.add_string buf "false"
  | Eq (t, m) -> Printf.bprintf buf "%a = %a" T.pp t Monome.pp m
  | Neq (t, m) -> Printf.bprintf buf "%a ≠ %a" T.pp t Monome.pp m
  | L_less (t, m) -> Printf.bprintf buf "%a < %a" T.pp t Monome.pp m
  | L_lesseq (t, m) -> Printf.bprintf buf "%a ≤ %a" T.pp t Monome.pp m
  | R_less (m, t) -> Printf.bprintf buf "%a < %a" Monome.pp m T.pp t
  | R_lesseq (m, t) -> Printf.bprintf buf "%a ≤ %a" Monome.pp m T.pp t

  let to_string lit = Util.on_buffer pp lit

  let is_arith lit = match lit with
  | Literal.Equation (l, r, _, _) -> T.is_arith l || T.is_arith r
  | Literal.Prop (p, _) -> T.is_arith p
  | Literal.True
  | Literal.False -> false

  exception TrivialLit
  exception UnsatLit

  let extract ~signature lit =
    Util.enter_prof prof_arith_extract;
    (* extract literal from (l=r | l!=r) *)
    let extract_eqn l r sign =
      try
        let m1 = Monome.of_term ~signature l in
        let m2 = Monome.of_term ~signature r in
        let m = Monome.difference m1 m2 in
        (* remove denominator, it doesn't matter *)
        let m = Monome.product m m.Monome.divby in
        let terms = Monome.to_list m in
        if Monome.is_constant m
        then if Monome.sign m = 0
          then if sign then [True] else [False]
          else if sign then [False] else [True]
        (* for each term, pivot the monome so that we isolate the term
          on one side of the (dis)equation, but only if it admits solutions *)
        else List.map
          (fun (coeff, t) ->
            assert (not (S.Arith.is_zero coeff));
            let m = Monome.divby (Monome.remove m t) (S.Arith.Op.abs coeff) in
            (* -t+m = 0 ---> t=m, but t+m = 0 ----> t=-m *)
            let m = if S.Arith.sign coeff < 0 then m else Monome.uminus m in
            if sign
              then if Monome.has_instances m
                then Eq (t, m)
              else raise UnsatLit  (* unsatisfiable diophantine eq *)
            else if Monome.has_instances m
              then Neq (t, m)
              else raise TrivialLit (* always true, diophantine eq has no solution *)
          )
          terms
      with
      | Monome.NotLinear -> []
      | TrivialLit -> [True]
      | UnsatLit -> [False]
    (* extract lit from (l <= r | l < r) *)
    and extract_less ~strict l r =
      try
        let m1 = Monome.of_term ~signature l in
        let m2 = Monome.of_term ~signature r in
        let m = Monome.difference m1 m2 in
        (* remove the denominator *)
        assert (S.Arith.sign m.Monome.divby > 0);
        let m = Monome.product m m.Monome.divby in
        let terms = Monome.to_list m in
        if terms = []
        (* constant, ground arith expression, must be true or false *)
        then match Monome.sign m with
          | 0 -> if strict then [False] else [True]
          | n when n < 0 -> [True]
          | _ -> [False]  (* m < 0 where m>0 *)
        (* l <| r is equivalent to m <| 0.
            for each term [t] of [m], pivot the monome to isolate [t]. Careful
              with the sign as it can change the comparison sign too! If the
              coeff of [t] is > 0 it means that the term [t] is on the {b left}
            side. *)
        else List.map
          (fun (coeff, t) ->
            assert (not (S.Arith.is_zero coeff));
            (* do we have to change the sign of comparison? *)
            let swap = S.Arith.sign coeff < 0 in
            let m = Monome.divby (Monome.remove m t) (S.Arith.Op.abs coeff) in
            match strict, swap with
            | true, false ->
              L_less (t, Monome.uminus m) (* t+m < 0 ---> t < -m *)
            | true, true ->
              R_less (m, t)  (* -t+m < 0 ---> m < t *)
            | false, false ->
              L_lesseq (t, Monome.uminus m)  (* t+m <= 0 ---> t <= -m *)
            | false, true ->
              R_lesseq (m, t)  (* -t+m <= 0 ---> m <= t *)
          )
          terms
      with Monome.NotLinear -> []
    in
    let extract_le a b = extract_less ~strict:false a b in
    let extract_lt a b = extract_less ~strict:true a b in
    let ans = match lit with
    | Literal.True
    | Literal.False -> []
    | Literal.Equation (l, r, sign, _) ->
      if T.is_arith l || T.is_arith r
        then extract_eqn l r sign
        else []
    | Literal.Prop ({T.term=T.Node (S.Const ("$less",_), [a; b])}, true) ->
      extract_lt a b
    | Literal.Prop ({T.term=T.Node (S.Const ("$less",_), [a; b])}, false) ->
      extract_le b a
    | Literal.Prop ({T.term=T.Node (S.Const ("$lesseq",_), [a; b])}, true) ->
      extract_le a b
    | Literal.Prop ({T.term=T.Node (S.Const ("$lesseq",_), [a; b])}, false) ->
      extract_lt b a
    | Literal.Prop ({T.term=T.Node (S.Const ("$greater",_), [a; b])}, true) ->
      extract_lt b a
    | Literal.Prop ({T.term=T.Node (S.Const ("$greater",_), [a; b])}, false) ->
      extract_le a b
    | Literal.Prop ({T.term=T.Node (S.Const ("$greatereq",_), [a; b])}, true) ->
      extract_le b a
    | Literal.Prop ({T.term=T.Node (S.Const ("$greatereq",_), [a; b])}, false) ->
      extract_lt a b
    | Literal.Prop _ -> []
    in
    Util.debug 5 "arith extraction of %a gives [%a]" Literal.pp lit (Util.pp_list pp) ans;
    Util.exit_prof prof_arith_extract;
    ans

  let to_lit ~ord lit =
    match lit with
    | True -> Literal.mk_tauto
    | False -> Literal.mk_absurd
    | Eq (t, m) ->
      Literal.mk_eq ~ord t (Monome.to_term m)
    | Neq (t, m) ->
      Literal.mk_neq ~ord t (Monome.to_term m)
    | L_less (t, m) ->
      Literal.mk_true (T.mk_less t (Monome.to_term m))
    | L_lesseq (t, m) ->
      Literal.mk_true (T.mk_lesseq t (Monome.to_term m))
    | R_less (m, t) ->
      Literal.mk_true (T.mk_less (Monome.to_term m) t)
    | R_lesseq (m, t) ->
      Literal.mk_true (T.mk_lesseq (Monome.to_term m) t)

  let simplify ~ord ~signature lit =
    (* simplify [l <= r] (depends on [strict]) *)
    let _simpl_less l r strict =
      let ty = TypeInference.FO.infer_sig signature l in
      let p = if strict && Type.eq ty Type.int
        then
          (* l < r  rewritten into l <= r-1, for integers *)
          let p = T.mk_lesseq l (T.mk_difference r (T.mk_const (S.Arith.one_i))) in
          T.simplify ~signature p
        else if strict
          then T.mk_less l r
        else T.mk_lesseq l r
      in
      Literal.mk_true p
    in
    let lit = match lit with
    | Literal.Prop (p, sign) ->
      let p' = T.simplify ~signature p in
      begin match p'.T.term, sign with
      | T.Node (S.Const("$less",_), [l;r]), false ->
        (* not (l < r) ---> r <= l *)
        _simpl_less r l false
      | T.Node (S.Const("$lesseq",_), [l;r]), false ->
        (* not (l <= r) ---> r < l *)
        _simpl_less r l true
      | T.Node (S.Const("$lesseq",_), [l;r]), true ->
        _simpl_less l r false
      | T.Node (S.Const("$less",_), [l;r]), true ->
        _simpl_less l r true
      | _ -> Literal.mk_prop p' sign
      end
    | Literal.Equation (l, r, sign, _) ->
      let l' = T.simplify ~signature l in
      let r' = T.simplify ~signature r in
      begin match sign with
      | true when T.is_arith_const l'
        && T.is_arith_const r' && not (T.eq l' r') ->
        (* i = j, when i and j are distinct numbers ---> false *)
        Literal.mk_false T.true_term
      | false when T.is_arith_const l'
        && T.is_arith_const r' && not (T.eq l' r') ->
        (* i != j. when i and j are distinct numbers ---> true *)
        Literal.mk_true T.true_term
      | _ ->
        (* just keep the literal *)
        Literal.mk_lit ~ord l' r' sign 
      end
    | Literal.True
    | Literal.False -> lit
    in
    match extract ~signature lit with
    | [True] | [False] -> lit  (* already simplified *)
    | [lit'] ->
      (* exactly one pivot possible, apply it! *)
      to_lit ~ord lit'
    | _ -> lit  (* keep lit *)

  let is_trivial ~signature lit =
    let l = extract ~signature lit in
    List.exists
      (function
      | True -> true
      | _ -> false)
      l

  let has_instances ~signature lit =
    let l = extract ~signature lit in
    List.for_all
      (function
      | False -> false
      | _ -> true)
      l

  let get_term = function
  | True
  | False -> invalid_arg "get_term"
  | Eq (t, _)
  | Neq (t, _)
  | L_less (t, _)
  | L_lesseq (t, _)
  | R_less (_, t)
  | R_lesseq (_, t) -> t

  let get_monome = function
  | True
  | False -> invalid_arg "get_monome"
  | Eq (_, m)
  | Neq (_, m)
  | L_less (_, m)
  | L_lesseq (_, m)
  | R_less (m, _)
  | R_lesseq (m, _) -> m

  (* unify non-arith subterms pairwise *)
  let factor lit =
    let l = get_term lit :: Monome.terms (get_monome lit) in
    let l = Util.list_diagonal l in
    Util.list_fmap
      (fun (t1, t2) ->
        try Some (FOUnif.unification t1 0 t2 0)
        with FOUnif.Fail -> None)
      l

  (* TODO: use diophantine equations! See the module {! Monome.Solve} *)

  (* find instances of variables that eliminate the literal *)
  let eliminate ?(elim_var=(fun v -> true)) ~signature lit =
    (* unify [t] with monome [m], but only if [m] has instances *)
    let unif_arith t1 sc_t m sc_m =
      if not (Monome.has_instances m)
        then raise FOUnif.Fail;
      FOUnif.unification t1 sc_t (Monome.to_term m) sc_m
    in
    begin match lit with
    | True
    | False -> []
    | Eq (x, m) when not (T.is_var x) || elim_var x -> 
      (* x = m eliminated with x := m+1 *)
      begin try
        [ unif_arith x 0 (Monome.pred m) 0]
      with FOUnif.Fail -> []  (* occur check... *)
      end
    | Neq (x, m) when not (T.is_var x) || elim_var x ->
      begin try
        [ unif_arith x 0 m 0 ]
      with FOUnif.Fail -> []
      end
    | L_less(x, m) when not (T.is_var x) || elim_var x ->
      (* x < m  is inconsistent with x = ceil(m) *)
      begin try
        [ unif_arith x 0 (Monome.ceil m) 0]
      with FOUnif.Fail -> []  (* occur check... *)
      end
    | R_less(m, x) when not (T.is_var x) || elim_var x ->
      (* m < x  is inconsistent with x = floor(m) *)
      begin try
        [ unif_arith x 0 (Monome.floor m) 0]
      with FOUnif.Fail -> []  (* occur check... *)
      end
    | L_lesseq(x, m) when not (T.is_var x) || elim_var x ->
      (* x <= m inconsistent with x = ceil(m)+1 *)
      begin try
        [ unif_arith x 0 Monome.(succ (ceil m)) 0 ]
      with FOUnif.Fail -> []  (* occur check... *)
      end
    | R_lesseq(m, x) when not (T.is_var x) || elim_var x ->
      (* x >= m inconsistent with x = floor(m)-1 *)
      begin try
        [ unif_arith x 0 Monome.(pred (floor m)) 0 ]
      with FOUnif.Fail -> []  (* occur check... *)
      end
    | _ -> []
    end

  let heuristic_eliminate ~signature lit =
    match lit with
    | ( Literal.Equation ({T.term=T.Node(prod, [x1; x2])}, {T.term=T.Node(n,[])}, false, _)
      | Literal.Equation ({T.term=T.Node(n,[])}, {T.term=T.Node(prod, [x1; x2])}, false, _))
      when S.eq prod S.Arith.product && T.is_var x1 && T.eq x1 x2 && S.is_numeric n ->
      (* ahah, square root spotted! *)
      Util.debug 5 "heuristic_elim tries sqrt of %a" S.pp n;
      begin match n with
      | S.Int n ->
        if Big_int.sign_big_int n >= 0
          then
            let s = Big_int.sqrt_big_int n in
            if Big_int.eq_big_int (Big_int.square_big_int s) n
              then
                (* n is positive, and has an exact square root, try both
                    the positive and negative square roots*)
                [ Substs.FO.bind Substs.FO.empty x1 0 (T.mk_const (S.mk_bigint s)) 0
                ; Substs.FO.bind Substs.FO.empty x1 0
                  (T.mk_const (S.mk_bigint (Big_int.minus_big_int s))) 0 ]
              else []
          else []
      | S.Rat n -> []  (* TODO *)
      | S.Real n ->
        if n >= 0.
          then
            let s = sqrt n in
            [ Substs.FO.bind Substs.FO.empty x1 0 (T.mk_const (S.mk_real s)) 0
            ; Substs.FO.bind Substs.FO.empty x1 0 (T.mk_const (S.mk_real (~-. s))) 0
            ]
          else []
      | _ -> failwith "unknown numeric type!?"
      end
    | _ -> []

  module L = struct
    let get_terms l = match l with
    | [True]
    | [False] -> []
    | l -> List.map get_term l

    let filter l p =
      List.filter
        (fun lit ->
          try
            let t = get_term lit in
            let m = get_monome lit in
            p t m
          with Invalid_argument _ -> false)
        l
  end
end

(** {2 Arrays of literals} *)

module Lits = struct
  let purify ~ord ~signature ~eligible lits =
    let new_lits = ref [] in
    let _add_lit lit = new_lits := lit :: !new_lits in
    let varidx = ref (T.max_var (Literals.vars lits) + 1) in
    (* purify a term (adding constraints to the list). [root] is true only
        if the term occurs in the outermost arith expression *)
    let rec purify_term ~root t = match t.T.term with
    | T.Var _
    | T.BoundVar _ -> t
    | T.Node (s,[]) when S.is_numeric s -> t
    | T.Node (s, l) when S.Arith.is_arith s ->
      if root
        then (* recurse, still in root arith expression *)
          T.mk_node s (List.map (purify_term ~root) l)
        else begin
          (* purify this term out! *)
          let ty = TypeInference.FO.infer_sig signature t in
          let v = T.mk_var ~ty !varidx in
          incr varidx;
          (* purify the term and add a constraint *)
          let t' = purify_term ~root:true t in
          let lit = Literal.mk_neq ~ord v t' in
          _add_lit lit;
          (* return variable instead of literal *)
          v
        end
    | T.Node (s, l) -> T.mk_node s (List.map (purify_term ~root:false) l)
    in
    (* purify each literal *)
    Array.iteri
      (fun i lit ->
        if eligible i lit
          then match lit with
          | Literal.Equation (l, r, sign, _) ->
            let l = purify_term ~root:true l in
            let r = purify_term r ~root:true in
            let lit = Literal.mk_lit ~ord l r sign in
            _add_lit lit
          | Literal.Prop (p, sign) ->
            let p = purify_term ~root:true p in
            let lit = Literal.mk_prop p sign in
            _add_lit lit
          | Literal.True -> _add_lit lit
          | Literal.False -> ()  (* useless *)
          else _add_lit lit (* keep *)
      )
      lits;
    Array.of_list (List.rev !new_lits)

  let pivot ~ord ~signature ~eligible lits =
    let results = ref [] in
    let add_res a = results := a :: !results in
    for i = 0 to Array.length lits - 1 do
      if eligible i lits.(i) then begin
        (* try to pivot the i-th literal *)
        let pivots = Lit.extract ~signature lits.(i) in
        (* only keep maximal terms *)
        let terms = Lit.L.get_terms pivots in
        let terms = Multiset.create terms in
        let bv = Multiset.max (Ordering.compare ord) terms in
        let terms = BV.select bv (Multiset.to_array terms) in
        List.iter
          (fun lit' ->
            (* build a new literal from lit', if the term is maximal *)
            let t = Lit.get_term lit' in
            if List.exists (fun (t',_) -> T.eq t t') terms then
              let lits = Util.array_except_idx lits i in
              let lits = Lit.to_lit ~ord lit' :: lits in
              let lits = Array.of_list lits in
              add_res lits
          )
          pivots
      end
    done;
    !results

  let shielded ?(filter=(fun _ _ -> true)) lits v =
    if not (T.is_var v) then failwith "shielded: need a var";
    try
      for i = 0 to Array.length lits - 1 do
        if filter i lits.(i) &&
        match lits.(i) with
        | Literal.Prop (p, _) -> T.shielded v p
        | Literal.Equation (l, r, _, _) -> T.shielded v l || T.shielded v r
        | _ -> false
        then raise Exit
      done;
      false
    with Exit ->
      true

  let naked_vars ?filter lits =
    let vars = Literals.vars lits in
    List.filter (fun v -> not (shielded ?filter lits v)) vars

  let eliminate ~ord ~signature ~eligible lits =
    let results = ref [] in
    let lits' = Array.to_list lits in
    let add_res a = results := a :: !results in
    (* instantiate with [subst]. Simplifications should then remove
        the literal; making the instantiation a step makes the proof
        more readable *)
    let eliminate_lit i subst =
      let renaming = Substs.FO.Renaming.create 5 in
      let lits' = Literal.apply_subst_list ~ord ~renaming subst lits' 0 in
      let lits' = Array.of_list lits' in
      add_res lits'
    in
    for i = 0 to Array.length lits - 1 do
      if eligible i lits.(i) then begin
        (* can eliminate only naked vars *)
        let elim_var =
          let vars = naked_vars ~filter:(fun i' _ -> i<>i') lits in
          fun v -> List.memq v vars
        in
        (* try heuristic substitutions *)
        let substs = Lit.heuristic_eliminate ~signature lits.(i) in
        (* try to extract arithmetic literals, then eliminate them *)
        let arith_lits = Lit.extract ~signature lits.(i) in
        let substs = List.fold_left
          (fun substs arith_lit ->
            Lit.eliminate ~elim_var ~signature arith_lit @ substs)
          substs arith_lits
        in
        List.iter
          (fun subst -> eliminate_lit i subst)
          substs;
      end
    done;
    !results
end
