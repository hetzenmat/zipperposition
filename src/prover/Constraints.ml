open Logtk
open Future

module T = Term
module US = Unif_subst
module H = HVar
module U = Unif_subst

type subst = US.t

type t = Unif_constr.t list

let _NOT_IMPLEMENTED : type a. a = assert false

let mk_empty = []

(** Are the constraints solvable?
      
    This should be a sound approximation (not complete).
    So we only check if only flex-flex pairs are present.

    That is: iterate all pairs and if the heads are variables and not referenced in 
  *)
let solvable _t = true (** TODO [MH] *)

module Make (St : sig val st : Flex_state.t end) = struct
  module PUP = PragUnifParams 
  module SU = SolidUnif.Make(St)

  let get_option k = Flex_state.get_exn k St.st 

  let renamer ~counter t0s t1s = 
    let lhs,rhs, unifscope, us = U.FO.rename_to_new_scope ~counter t0s t1s in
    lhs,rhs,unifscope,U.subst us

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

let unify_scoped =  
  let counter = ref 0 in

  let module PreUnifParams = struct
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
  end in

  let module PreUnif = UnifFramework.Make(PreUnifParams) in
  PreUnif.unify_scoped

  let unify_scoped_l =  
    let counter = ref 0 in
  
    let module PreUnifParams = struct
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
    end in
  
    let module PreUnif = UnifFramework.Make(PreUnifParams) in
    PreUnif.unify_scoped_l

end