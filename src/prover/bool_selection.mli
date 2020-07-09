
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean selection functions} *)

open Logtk

(** As described in FBoolSup paper, Boolean selection function
    selects positions in the clause that are non-interpreted 
    Boolean subterms. *)

type t = Literal.t array -> Position.t list

type parametrized = strict:bool -> ord:Ordering.t -> t

val no_select : t
(** Never select Boolean subterms *)

(** {2 Selection Functions} *)

val from_string : ord:Ordering.t -> string -> t
(** selection function from string (may fail) *)

val all : unit -> string list
(** available names for selection functions *)