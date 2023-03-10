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
        assert false (* TODO [MH] handle flex-flex *)
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
    let init_flag = (0:flag_type)
    let identify_scope = renamer ~counter
    let identify_scope_l = renamer_l ~counter
    let frag_algs = deciders ~counter
    let pb_oracle s t (f:flag_type) _ scope = 
      oracle ~counter ~scope s t f
  end in

  let module PreUnif = UnifFramework.Make(PreUnifParams) in
  (fun x y ->
      let res = PreUnif.unify_scoped x y in
      OSeq.map (CCOpt.map Unif_subst.of_subst) res)
  end

let imitate ~scope ~counter var_headed_term other_term =
  assert (Type.equal (T.ty var_headed_term) (T.ty other_term));
  
  let head_l = T.head_term_mono var_headed_term in

  assert (T.is_var head_l); (* left term must be variable-headed *)

  let head_r = T.head_term_mono other_term in

  assert (not (T.is_bvar head_r) && not (T.is_fun head_r)); (* head of right term must not be a bound variable nor a λ-expression *)

  let prefix_types_l, _ = Type.open_fun (T.ty head_l) in
  let prefix_types_r, _ = Type.open_fun (T.ty head_r) in
  
  (* create substitution: head_l |-> λ u1 ... um. head_r (x1 u1 ... um) ... (xn u1 ... um)) *)
  let bvars = prefix_types_l |> List.rev |> FList.mapi (fun i ty -> T.bvar ~ty i) |> List.rev in
  let matrix_args = 
    prefix_types_r 
    |> FList.map (fun prefix_type_r ->
        let ty = Type.arrow prefix_types_l prefix_type_r in
        let var = T.var (H.fresh_cnt ~counter ~ty ()) in
        T.app var bvars) 
  in
  let matrix = T.app head_r matrix_args in
  let subst_value = T.fun_l prefix_types_l matrix in 
  assert (T.DB.is_closed subst_value);
  US.FO.singleton (T.as_var_exn head_l, scope) (subst_value, scope)

let project ~scope ~counter var_headed_term other_term =
  assert (Type.equal (T.ty var_headed_term) (T.ty other_term));

  let head_l = T.head_term_mono var_headed_term in

  assert (T.is_var head_l); (* left term must be variable-headed *)

  let head_r = T.head_term_mono other_term in
  (scope, counter, head_r) |> (fun _ -> ());
  () (* TODO [MH] *)