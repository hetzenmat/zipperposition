(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** Heuristic selection of clauses, using queues. Note that some
    queues do not need accept all clauses, as long as one of them does
    (for completeness). Anyway, a fifo queue should always be present,
    and presents this property. *)

open Types
open Hashcons

module C = Clauses
module O = Orderings
module Utils = FoUtils

(** A priority queue of clauses, purely functional *)
class type queue =
  object
    method add : hclause -> queue
    method add_list : hclause list -> queue
    method is_empty: bool
    method take_first : (queue * hclause)
    method clean : C.CSet.t -> queue    (** remove all clauses that are not in the set *)
    method name : string
  end

(** A heap that compares clauses by their weight *)
module LH = Leftistheap.Make(
  struct
    type t = int * hclause
    let le (i, _) (j, _) = i <= j
  end)

(** generic clause queue based on some ordering on clauses, given
    by a weight function (TODO rewrite LH to store weight in tree nodes?) *)
let make_hq ?(accept=(fun _ -> true)) ~weight name =
  object
    val heap = LH.empty
    
    method is_empty = LH.is_empty heap

    method add hc =
      if accept hc then
        let w = weight hc in
        let new_heap = LH.insert (w, hc) heap in
        ({< heap = new_heap >} :> queue)
      else
        ({<>} :> queue)

    method add_list hcs =
      (* add clauses to the heap *)
      let new_heap = List.fold_left
        (fun heap hc ->
          if accept hc
            then LH.insert (weight hc, hc) heap
            else heap)
        heap hcs
      in 
      ({< heap = new_heap >} :> queue)

    method take_first =
      assert (not (LH.is_empty heap));
      let (_, c), new_h = LH.extract_min heap in
      (({< heap = new_h >} :> queue), c)

    (** Keep only the clauses that are in the set *)
    method clean set =
      let heap =
        LH.filter
          (fun (_, hc) -> C.CSet.mem hc set)
          heap
      in 
      ({< heap = new_h >} :> queue)

    method name = name
  end

let fifo =
  let name = "fifo_queue" in
  make_hq ~weight:(fun hc -> hc.ctag) name

let clause_weight =
  let name = "clause_weight" in
  make_hq ~weight:(fun hc -> hc.cweight) name
  
let goals =
  (* check whether a literal is a goal *)
  let is_goal_lit lit = match lit with
  | Equation (_, _, sign, _) -> not sign in
  let is_goal_clause clause = List.for_all is_goal_lit clause.clits in
  let name = "prefer_goals" in
  make_hq ~accept:is_goal_clause ~weight:(fun c -> c.cweight) name

let non_goals =
  (* check whether a literal is a goal *)
  let is_goal_lit lit = match lit with
  | Equation (_, _, sign, _) -> not sign in
  let is_non_goal_clause clause = List.for_all (fun x -> not (is_goal_lit x)) clause.clits in
  let name = "prefer_non_goals" in
  make_hq ~accept:is_non_goal_clause ~weight:(fun c -> c.cweight) name

let pos_unit_clauses =
  let is_unit_pos c = match c.clits with
  | [Equation (_,_,true,_)] -> true
  | _ -> false
  in
  let name = "prefer_pos_unit_clauses" in
  make_hq ~accept:is_unit_pos ~weight:(fun c -> c.cweight) name

let default_queues =
  [ (clause_weight, 4);
    (pos_unit_clauses, 3);
    (non_goals, 1);
    (goals, 1);
    (fifo, 1);
  ]

let pp_queue formatter q =
  Format.fprintf formatter "@[<h>queue %s@]" q#name

let pp_queue_weight formatter (q, w) =
  Format.fprintf formatter "@[<h>queue %s, %d@]" q#name w

let pp_queues formatter qs =
  Format.fprintf formatter "@[<hov>%a@]" (Utils.pp_list ~sep:"; " pp_queue_weight) qs

