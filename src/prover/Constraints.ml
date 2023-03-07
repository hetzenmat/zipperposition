open Logtk
open Future

module T = Term
module US = Unif_subst
module H = HVar

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

let unify_scoped ((_t0, _scope0) : T.t Scoped.t) ((_t1, _scope1) : T.t Scoped.t) : subst option OSeq.t =
  OSeq.empty (* TODO [MH] *)

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