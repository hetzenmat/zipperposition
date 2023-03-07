open Logtk

module T = Term
module US = Unif_subst

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