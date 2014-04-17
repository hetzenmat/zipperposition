
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


(** {1 Clauses} *)

(** The clauses are hashconsed within a context. The maximum number of literals
    that a clause can have is 63 (on a 64-bits machine), because of the way
    bitvectors are represented. A clause of more than 63 literals
    is replaced by "true" (tautology) and the context's incompleteness
    flag is set.
*)

open Logtk

val stat_fresh : Util.stat
val stat_mk_hclause : Util.stat
val stat_new_clause : Util.stat

type scope = Substs.scope

(** {2 Clauses that depend on a Context} *)
module type S = sig
  module Ctx : Ctx.S

  (* TODO: maybe a global (weak) graph of parents/descendants
   * would be leaner on memory? *)

  type t
  type clause = t

  (** {2 Flags} *)

  val flag_ground : int                       (** clause is ground *)
  val flag_lemma : int                        (** clause is a lemma *)
  val flag_persistent : int                   (** clause cannot be redundant *)

  val set_flag : int -> t -> bool -> unit     (** set boolean flag *)
  val get_flag : int -> t -> bool             (** get value of boolean flag *)
  val new_flag : unit -> int                  (** new flag that can be used on clauses *)

  (** {2 Basics} *)

  val eq : t -> t -> bool         (** equality of clauses *)
  val hash : t -> int             (** hash a clause *)
  val compare : t -> t -> int     (** simple order on clauses (by ID) *)

  val id : t -> int
  val lits : t -> Literal.t array
  val parents : t -> t list

  val compact : t -> CompactClause.t (** Turn into a compact clause *)
  val is_ground : t -> bool
  val weight : t -> int

  module CHashtbl : Hashtbl.S with type key = t

  module CHashSet : sig
    type t
    val create : unit -> t
    val is_empty : t -> bool
    val member : t -> clause -> bool
    val iter : t -> (clause -> unit) -> unit
    val add : t -> clause -> unit
    val to_list : t -> clause list
  end

  val is_child_of : child:t -> t -> unit
    (** [is_child_of ~child c] is to be called to remember that [child] is a child
        of [c], is has been infered/simplified from [c] *)

  val follow_simpl : t -> t
    (** Follow the "hcsimplto" links until the clause has None *)

  val simpl_to : from:t -> into:t -> unit
    (** [simpl_to ~from ~into] sets the link of [from] to [into], so that
        the simplification of [from] into [into] is cached. *)

  module CHashcons : Hashcons.S with type elt = clause

  val create : ?parents:t list -> ?selected:BV.t ->
               Literal.t list ->
               (CompactClause.t -> Proof.t) -> t
    (** Build a new hclause from the given literals. *)

  val create_a : ?parents:t list -> ?selected:BV.t ->
                  Literal.t array ->
                  (CompactClause.t -> Proof.t) -> t
    (** Build a new hclause from the given literals. This function takes
        ownership of the input array. *)

  val of_forms : ?parents:t list -> ?selected:BV.t ->
                      Formula.FO.t list ->
                      (CompactClause.t -> Proof.t) -> t
    (** Directly from list of formulas *)

  val of_forms_axiom : ?role:string -> file:string -> name:string ->
                       Formula.FO.t list -> t
    (** Construction from formulas as axiom (initial clause) *)

  val proof : t -> Proof.t
    (** Extract its proof from the clause *)

  val stats : unit -> (int*int*int*int*int*int)
    (** hashconsing stats *)

  val is_empty : t -> bool
    (** Is the clause an empty clause? *)

  val length : t -> int
    (** Number of literals *)

  val descendants : t -> int SmallSet.t
    (** set of ID of descendants of the clause *)

  val apply_subst : renaming:Substs.Renaming.t -> Substs.t -> t -> scope -> t
    (** apply the substitution to the clause *)

  val maxlits : t -> scope -> Substs.t -> BV.t
    (** Bitvector that indicates which of the literals of [subst(clause)]
        are maximal under [ord] *)

  val is_maxlit : t -> scope -> Substs.t -> int -> bool
    (** Is the i-th literal maximal in subst(clause)? Equivalent to
        Bitvector.get (maxlits ~ord c subst) i *)

  val eligible_res : t -> scope -> Substs.t -> BV.t
    (** Bitvector that indicates which of the literals of [subst(clause)]
        are eligible for resolution. THe literal has to be either maximal
        among selected literals of the same sign, if some literal is selected,
        or maximal if none is selected. *)

  val eligible_param : t -> scope -> Substs.t -> BV.t
    (** Bitvector that indicates which of the literals of [subst(clause)]
        are eligible for paramodulation. That means the literal
        is positive, no literal is selecteed, and the literal
        is maximal among literals of [subst(clause)]. *)

  val eligible_chaining : t -> scope -> Substs.t -> BV.t
    (** Bitvector of literals of [subst(clause)] that are eligible
        for equality chaining or inequality chaining. That amouns to being
        a maximal, positive inequality literal within the clause,
        and assume the clause has no selected literal. *)

  val has_selected_lits : t -> bool
    (** does the clause have some selected literals? *)

  val is_selected : t -> int -> bool
    (** check whether a literal is selected *)

  val selected_lits : t -> (Literal.t * int) list
    (** get the list of selected literals *)

  val is_unit_clause : t -> bool
    (** is the clause a unit clause? *)

  val is_oriented_rule : t -> bool
    (** Is the clause a positive oriented clause? *)

  val symbols : ?init:Symbol.Set.t -> t Sequence.t -> Symbol.Set.t
    (** symbols that occur in the clause *)

  module Seq : sig
    val lits : t -> Literal.t Sequence.t
    val terms : t -> FOTerm.t Sequence.t
    val vars : t -> FOTerm.t Sequence.t

    val abstract : t -> (bool * FOTerm.t Sequence.t) Sequence.t
      (** Easy iteration on an abstract view of literals *)
  end

  (** {2 Filter literals} *)

  module Eligible : sig
    type t = int -> Literal.t -> bool
      (** Eligibility criterion for a literal *)

    val res : clause -> t
      (** Only literals that are eligible for resolution *)

    val param : clause -> t
      (** Only literals that are eligible for paramodulation *)

    val chaining : clause -> t
      (** Eligible for chaining *)

    val eq : t
      (** Equations *)

    val ineq : clause -> t
      (** Only literals that are inequations *)

    val ineq_of : clause -> Theories.TotalOrder.t -> t
      (** Only literals that are inequations for the given ordering *)

    val max : clause -> t
      (** Maximal literals of the clause *)

    val pos : t
      (** Only positive literals *)

    val neg : t
      (** Only negative literals *)

    val always : t
      (** All literals *)

    val combine : t list -> t
      (** Logical "and" of the given eligibility criteria. A literal is
          eligible only if all elements of the list say so. *)
  end

  (** {2 Set of clauses} *)

  (** Simple set *)
  module ClauseSet : Set.S with type elt = t

  (** Set with access by ID, bookeeping of maximal var... *)
  module CSet : sig
    (** Set of hashconsed clauses. *)
    type t

    val empty : t
      (** the empty set *)

    val is_empty : t -> bool
      (** is the set empty? *)

    val size : t -> int
      (** number of clauses in the set *)

    val add : t -> clause -> t
      (** add the clause to the set *)

    val add_list : t -> clause list -> t
      (** add several clauses to the set *)

    val remove_id : t -> int -> t
      (** remove clause by ID *)

    val remove : t -> clause -> t
      (** remove hclause *)

    val remove_list : t -> clause list -> t
      (** remove hclauses *)

    val get : t -> int -> clause
      (** get a clause by its ID *)

    val mem : t -> clause -> bool
      (** membership test *)

    val mem_id : t -> int -> bool
      (** membership test by t ID *)

    val choose : t -> clause option
      (** Choose a clause in the set *)

    val union : t -> t -> t
      (** Union of sets *)

    val inter : t -> t -> t
      (** Intersection of sets *)

    val iter : t -> (clause -> unit) -> unit
      (** iterate on clauses in the set *)

    val iteri : t -> (int -> clause -> unit) -> unit
      (** iterate on clauses in the set with their ID *)

    val fold : t -> 'b -> ('b -> int -> clause -> 'b) -> 'b
      (** fold on clauses *)

    val to_list : t -> clause list
    val of_list : clause list -> t

    val to_seq : t -> clause Sequence.t
    val of_seq : t -> clause Sequence.t -> t
    val remove_seq : t -> clause Sequence.t -> t
    val remove_id_seq : t -> int Sequence.t -> t
  end

  (** {2 Position} *)

  module Pos : sig
    val at : t -> Position.t -> FOTerm.t
  end

  (** {2 Clauses with more data} *)

  (** Clause within which a subterm (and its position) are hilighted *)
  module WithPos : sig
    type t = {
      clause : clause;
      pos : Position.t;
      term : FOTerm.t;
    }

    val compare : t -> t -> int
    val pp : Buffer.t -> t -> unit
  end

  (** {2 IO} *)

  val pp : Buffer.t -> t -> unit
  val pp_tstp : Buffer.t -> t -> unit
  val pp_tstp_full : Buffer.t -> t -> unit  (** Print in a cnf() statement *)

  val to_string : t -> string               (** Debug printing to a  string *)
  val fmt : Format.formatter -> t -> unit   (** debug printing *)

  val pp_set : Buffer.t -> CSet.t -> unit
  val pp_set_tstp : Buffer.t -> CSet.t -> unit
end

module Make(Ctx : Ctx.S) : S with module Ctx = Ctx