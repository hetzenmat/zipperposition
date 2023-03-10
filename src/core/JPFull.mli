val imit_rule: counter:int ref -> scope:int -> Term.t -> Term.t -> int -> (Subst.t * int) option OSeq.t

val hs_proj_flex_rigid: counter:int ref -> scope:int -> flex:Term.t -> Term.t -> int -> (Subst.t * int) option OSeq.t

module Make (S: sig val st: Flex_state.t end) : UnifFramework.US
