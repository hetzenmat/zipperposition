open Logtk
open Future

module T = Term
module US = Unif_subst
module H = HVar
module U = Unif_subst
module Lit = Literal

type subst = US.t

type elem = T.t * T.t

type t = elem list

let mk_empty = []

let merge l r = l @ r 
let add e l = e :: l

let is_flex_flex (e: elem): bool =
  let head t = t |> Term.open_fun |> snd |> Term.head_term in
  let (lhs, rhs) = e in
  let hd_lhs = head lhs in
  let hd_rhs = head rhs in
  T.is_var hd_lhs && T.is_var hd_rhs

let apply_subst ~(renaming : Subst.Renaming.t) ~(subst: Subst.t) ((constraints, scope): t Scoped.t): t =
  let do_sub = Subst.FO.apply renaming subst in
  FList.map (fun (l,r) -> do_sub (l,scope), do_sub (r,scope)) constraints

let get_constraint renaming subst constr =
  let s = Subst.FO.apply renaming subst in
  (s (Unif_constr.get_scoped_t1 constr), s (Unif_constr.get_scoped_t2 constr))

let get_constraints renaming us =
  FList.map (get_constraint renaming (Unif_subst.subst us)) (Unif_subst.constr_l us)

let update_constraints renaming constraints us =
  let subst = Unif_subst.subst us in
  let constr_l = Unif_subst.constr_l us in
  let do_sub (t,sc) = Subst.FO.apply renaming subst (t,sc) in
  let constr_l = FList.map (fun p -> (do_sub (Unif_constr.get_scoped_t2 p), do_sub (Unif_constr.get_scoped_t2 p))) constr_l in
  let subst_constraints = apply_subst ~renaming ~subst constraints in
  merge subst_constraints constr_l

let vars (constraints : t) =
  Iter.of_list constraints
|> Iter.flat_map (fun (t1,t2) -> Iter.append (T.Seq.vars t1) (T.Seq.vars t2))
|> T.VarSet.of_iter
|> T.VarSet.to_list

let pp_single out ((t1,t2) : elem) =
  Lit.pp out (Lit.mk_eq t1 t2)

let pp out constraints =
  if not (CCList.is_empty constraints) then
    Format.fprintf out "{{%a}}" (Util.pp_list ~sep:", " pp_single) constraints

let to_string = CCFormat.to_string pp

let renamer ~counter t0s t1s = 
  let lhs,rhs, unifscope, us = U.FO.rename_to_new_scope ~counter t0s t1s in
  lhs,rhs,unifscope,U.subst us

module Make (St : sig val st : Flex_state.t end) = struct
  module PUP = PragUnifParams 
  module SU = SolidUnif.Make(St)

  let get_option k = Flex_state.get_exn k St.st 

  let renamer_l ~counter t0s t1s = 
    let lhs,rhs, unifscope, us = U.FO.rename_l_to_new_scope ~counter t0s t1s in
    lhs,rhs,unifscope,U.subst us

  let deciders ~counter () =
    let pattern = 
      if get_option PUP.k_pattern_decider then 
        [(fun s t sub -> [(U.subst @@ PatternUnif.unify_scoped ~subst:(U.of_subst sub) ~counter s t)])] 
      else [] in
    let solid = 
      if get_option PUP.k_solid_decider then 
        [(fun s t sub -> (FList.map U.subst @@ SU.unify_scoped ~subst:(U.of_subst sub) ~counter s t))] 
      else [] in
    let fixpoint = 
      if get_option PUP.k_fixpoint_decider then 
        [(fun s t sub -> [(U.subst @@ FixpointUnif.unify_scoped ~subst:(U.of_subst sub) ~counter s t)])] 
      else [] in
    fixpoint @ pattern @ solid

  let oracle ~counter ~scope (s,_) (t,_) depth =
    
    match Unif.head_classifier s, Unif.head_classifier t with 
      | `Flex _, `Flex _  ->
        assert false (* TODO [MH] should never happen *)
      | `Flex _, `Rigid
      | `Rigid, `Flex _ ->
        let flex, rigid = if Term.is_var (T.head_term s) then s,t else t,s in
        
        OSeq.append
          (JPFull.imit_rule ~counter ~scope s t depth)
          (JPFull.hs_proj_flex_rigid ~counter ~scope ~flex rigid depth) 
          
      | _ -> assert false

      let mkPreunifParams ~(counter: int ref): (module UnifFramework.PARAMETERS) = (
        module struct
          exception NotInFragment = PatternUnif.NotInFragment
          exception NotUnifiable = PatternUnif.NotUnifiable
          type flag_type = int
          let flex_state = St.st
          let preunification = true
          let init_flag = (0:flag_type)
          let identify_scope = renamer ~counter
          let identify_scope_l = renamer_l ~counter
          let frag_algs = deciders ~counter
          let pb_oracle s t (f:flag_type) _ scope = 
            oracle ~counter ~scope s t f
        end)

let unify_scoped =  
  let counter = ref 0 in
  let module PreUnifParams = (val mkPreunifParams ~counter) in

  let module PreUnif = UnifFramework.Make(PreUnifParams) in
  PreUnif.unify_scoped

let unify_scoped_l =  
  let counter = ref 0 in

  let module PreUnifParams = (val mkPreunifParams ~counter) in

  let module PreUnif = UnifFramework.Make(PreUnifParams) in
  PreUnif.unify_scoped_l
end

let unif_simple t s = 
  OSeq.return (try
    let type_unifier = Unif.FO.unify_syn ~subst:Subst.empty t s in
    Some type_unifier
  with Unif.Fail -> None)

let only_constraints ((t1,sc1) : T.t Scoped.t) ((t2,sc2) : T.t Scoped.t) : subst option OSeq.t =
  OSeq.map (function | None -> None
                     | Some s -> Some (US.make s [Unif_constr.make ~tags:[] ((t1 : T.t :> InnerTerm.t),sc1) ((t2 : T.t :> InnerTerm.t),sc2)]))
   (unif_simple ((Term.of_ty (T.ty t1)), sc1) ((Term.of_ty (T.ty t2)), sc2)) 

  