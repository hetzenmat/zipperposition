open Logtk

type t = Unif_constr.t list

let _NOT_IMPLEMENTED : type a. a = assert false

let mk_empty = []

(** Are the constraints solvable?
      
    This should be a sound approximation (not complete).
    So we only check if only flex-flex pairs are present.
  *)
let solvable _t = true (** TODO [MH] *)