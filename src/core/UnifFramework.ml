open Future

module S = Subst
module US = Unif_subst
module LL = OSeq
module T = Term
module U = Unif
module Q = CCDeque
module PUP = PragUnifParams

let section = Util.Section.make "unif.framework"

type frag_alg_t = T.t Scoped.t -> T.t Scoped.t -> S.t -> S.t list

module type PARAMETERS = sig
  exception NotInFragment
  exception NotUnifiable
  type flag_type
  val init_flag : flag_type
  val flex_state : Flex_state.t
  val preunification : bool
  val identify_scope : T.t Scoped.t -> T.t Scoped.t -> T.t * T.t * Scoped.scope * S.t
  val identify_scope_l : T.t list Scoped.t -> T.t list Scoped.t -> T.t list * T.t list * Scoped.scope * S.t
  val frag_algs : unit -> (string * frag_alg_t) list
  val pb_oracle : (T.t Scoped.t -> T.t Scoped.t -> flag_type -> S.t -> Scoped.scope -> (S.t * flag_type) option LL.t)
end

(*module type S = sig 
  val unify_scoped : T.t Scoped.t -> T.t Scoped.t -> S.FO.t option OSeq.t
  val unify_scoped_l : T.t list Scoped.t -> T.t list Scoped.t -> S.FO.t option OSeq.t
end*)

module type US = sig 
  val unify_scoped : T.t Scoped.t -> T.t Scoped.t -> US.t option OSeq.t
  val unify_scoped_l : T.t list Scoped.t -> T.t list Scoped.t -> US.t option OSeq.t
end

module Make (P : PARAMETERS) = struct 
  exception PolymorphismDetected

  let rec nfapply_mono subst (t,sc) =
    let pref, tt = T.open_fun t in
    let t' =  
      begin match T.view tt with
        | T.Var _ ->
          if not (Type.is_ground (T.ty tt)) then (
            raise PolymorphismDetected
          );

          let u, _ = S.FO.deref subst (tt,sc) in
          if T.equal tt u then u
          else nfapply_mono subst (u,sc)
        | T.App (f0, l) ->
          let f = nfapply_mono subst (f0, sc) in
          let t =
            if T.equal f0 f then tt else T.app f l in
          
          let u = Lambda.whnf t in
          if T.equal t u
          then t
          else nfapply_mono subst (u,sc)
        | _ -> tt
      end in
    if T.equal tt t' then t
    else T.fun_l pref t'

  (* apply a substitution, possibly eta-expand because
     a type substitution might introduce a need for expansion and reduce to whnf *)
  let nfapply s u = Lambda.whnf @@ S.FO.apply S.Renaming.none s u

  let normalize s u =
    try
      if not (Type.is_ground (T.ty (fst u))) then
        raise PolymorphismDetected;

      nfapply_mono s u 
    with PolymorphismDetected -> 
      nfapply s u 

  let eta_expand_otf pref1 pref2 t1 t2 =
    let do_exp_otf n types t = 
      let remaining = CCList.drop n types in
      assert(List.length remaining != 0);
      let num_vars = List.length remaining in
      let vars = FList.mapi (fun i ty -> 
          (* let ty = S.Ty.apply S.Renaming.none subst (ty,scope) in *)
          T.bvar ~ty (num_vars-1-i)) remaining in
      let shifted = T.DB.shift num_vars t in
      T.app shifted vars in

    if List.length pref1 = List.length pref2 then (t1, t2, pref1)
    else (
      let n1, n2 = List.length pref1, List.length pref2 in 
      if n1 < n2 then (do_exp_otf n1 pref2 t1,t2,pref2)
      else (t1,do_exp_otf n2 pref1 t2,pref1))

  let different_rigid_heads s t =
    not @@ T.is_var s && not @@ T.is_var t &&
    match T.view s with
    | T.DB _ -> not @@ T.is_bvar t
    | T.Const _  -> not @@ T.is_const t
    | T.AppBuiltin _ ->  not @@ T.is_appbuiltin t
    | _ -> false

  let do_unif ~bind_cnt ~hits_cnt problem subst unifscope =

    let classify_one s subst =
      let rec follow_bindings t =
        let hd = T.head_term @@ snd @@ (T.open_fun t) in
        let derefed,_ = Subst.FO.deref subst (hd, unifscope) in
        if T.equal hd derefed then hd
        else follow_bindings derefed in
  
      let hd = follow_bindings s in
  
      if T.is_const hd then `Const
      else if T.is_var hd then `Var
      (* when it is bound variable, we do not know what will 
          happen when it is reduced *)
      else `Unknown
    in

    let max_infs = 
      if Flex_state.get_exn PUP.k_max_inferences P.flex_state < 0 then max_int
      else Flex_state.get_exn PUP.k_max_inferences P.flex_state
    in

    let delay steps res () =
      let skipper = 
        int_of_float (Flex_state.get_exn PUP.k_skip_multiplier P.flex_state) in
      if steps != 0 && steps mod skipper == 0 then (
        OSeq.append (OSeq.of_list (CCList.replicate (steps / skipper) None)) res ()
      ) else res ()
    in

    let rec aux ?(root=false) subst problem =
      let decompose args_l args_r rest flag =
        let rec zipped_with_flag = function 
          | [], [] -> []
          | x::xs, y::ys -> (x,y,flag) :: (zipped_with_flag (xs,ys))
          | _, _ -> invalid_arg "lists must be of the same size." in

        let new_args = zipped_with_flag (args_l,args_r) in

        let to_classify, rest = 
          if List.length rest <= 15
          then new_args@rest, []
          else new_args, rest in

        let sort_class =
          List.sort (fun (l,r,_) (l', r',_) ->
              let l,l' = CCPair.map_same (fun t -> T.head_term @@ snd @@ T.open_fun t) (l,l') in
              let r,r' = CCPair.map_same (fun t -> T.head_term @@ snd @@ T.open_fun t) (r,r') in
              if (not (Term.is_app l) || not (Term.is_app r)) &&
                 (not (Term.is_app l') || not (Term.is_app r')) then 0
              else if not (Term.is_app l) || not (Term.is_app r) then -1
              else if not (Term.is_app l') || not (Term.is_app r') then 1
              else Term.ho_weight l + Term.ho_weight r - 
                   Term.ho_weight l' - Term.ho_weight r'
            ) in

        (* classifies the pairs as (rigid-rigid, flex-rigid, and flex-flex *)
        let rec classify = function 
          | ((lhs,rhs,_flag) as cstr) :: xs ->
            let rr,fr,unsure,ff = classify xs in
            begin match classify_one lhs subst, classify_one rhs subst with 
              | `Const, `Const -> cstr::rr,fr,unsure,ff
              | _, `Const  | `Const, _ -> rr, cstr::fr, unsure, ff
              | _, `Unknown | `Unknown, _ -> rr, fr, cstr::unsure, ff
              | `Var, `Var -> rr,fr,unsure, cstr::ff end
          | [] -> ([],[],[],[]) in

        let rr,fr,unsure,ff = classify to_classify in
        if Flex_state.get_exn PragUnifParams.k_sort_constraints P.flex_state then
          sort_class rr @ sort_class fr @ sort_class unsure @ rest @ sort_class ff
        else rr @ fr @ unsure @ rest @ ff in

      let decompose_and_cont ?inc_step:_ args_l args_r rest flag subst =
        let new_prob = decompose args_l args_r rest flag in
        (fun () -> aux subst new_prob ()) in

      let all_flex_flex prob =
        let is_flex_flex = fun (lhs, rhs, _) ->
          match classify_one lhs subst with
          | `Var -> (match classify_one rhs subst with
                     | `Var -> true
                     | _ -> false)
          | _ -> false
        in
        List.for_all is_flex_flex prob
      in

      let unif_types subst ?(scope = unifscope) t1 t2 =
        PatternUnif.unif_simple ~subst ((T.of_ty (T.ty t1)), scope) ((T.of_ty (T.ty t2)), scope)
      in

      let normalize_term subst t = normalize subst (t, unifscope) in

      let make_constraints =
        FList.map @@ fun (lhs, rhs) -> Unif_constr.make ~tags:[] ((lhs : T.t :> InnerTerm.t), unifscope)
                                                                 ((rhs : T.t :> InnerTerm.t), unifscope)
      in

      match problem with
      | _ when !hits_cnt > max_infs -> OSeq.empty
      | [] -> 
        incr hits_cnt;
        OSeq.return @@ Some (Unif_subst.of_subst subst)
      | _ when P.preunification && all_flex_flex problem ->

        let problem = FList.map (fun (l, r, _) -> (normalize_term subst l), (normalize_term subst r)) problem in

        let p = ref problem in
        let sub = ref subst in

        while !p != [] do
          let ((lhs, rhs), tail) = Future.head_tail !p in
          p := tail;

          match unif_types !sub lhs rhs with 
          | None -> raise Unif.Fail
          | Some subst ->
            sub := subst;
        done;

        let problem = FList.map (fun (l,r) -> normalize_term !sub l, normalize_term !sub r) problem in

        OSeq.return @@ Some (Unif_subst.make !sub (make_constraints problem))
      | (lhs, rhs, flag) as current_constraint :: rest ->
        match PatternUnif.unif_simple ~subst
                ((T.of_ty (T.ty lhs)), unifscope) ((T.of_ty (T.ty rhs)), unifscope) with 
        | None -> OSeq.empty
        | Some subst ->
          
          let (lhs, rhs) = 
            ( normalize subst (lhs, unifscope) 
            , normalize subst (rhs, unifscope) )
          in

          let (pref_lhs, body_lhs) = T.open_fun lhs
          and (pref_rhs, body_rhs) = T.open_fun rhs in 
          let body_lhs, body_rhs, _prefix_types = eta_expand_otf pref_lhs pref_rhs body_lhs body_rhs in
          let (hd_lhs, args_lhs), (hd_rhs, args_rhs) = T.as_app body_lhs, T.as_app body_rhs in


          (* assert that heads are in dereffed form wrt. subst *)
          let (==>) a b = not a || b in

          assert (T.is_var hd_lhs ==> Term.equal (fst @@ S.FO.deref subst (hd_lhs,unifscope)) hd_lhs);
          assert (T.is_var hd_rhs ==> Term.equal (fst @@ S.FO.deref subst (hd_rhs,unifscope)) hd_rhs);

          if Term.is_type lhs then (
            assert(Term.is_type rhs);
            try
              let subst = Unif.FO.unify_syn ~subst:(subst) (lhs, unifscope) (rhs, unifscope) in
              aux subst rest
            with Unif.Fail -> OSeq.empty
          ) else if T.equal body_lhs body_rhs then (
            aux subst rest
          ) else (
            match T.view hd_lhs, T.view hd_rhs with
            | T.DB i, T.DB j ->
              if i = j then decompose_and_cont args_lhs args_rhs rest flag subst
              else OSeq.empty
            | T.Const f, T.Const g ->
              if ID.equal f g && List.length args_lhs = List.length args_rhs 
              then decompose_and_cont args_lhs args_rhs rest flag subst
              else OSeq.empty
            | T.AppBuiltin(b1, args1), T.AppBuiltin(b2, args2) ->
              let args_lhs = args_lhs @ args1 and args_rhs = args_rhs @ args2 in
              if Builtin.equal b1 b2 then (
                try
                  let mode = Flex_state.get_exn PragUnifParams.k_logop_mode P.flex_state in
                  let args_lhs, args_rhs = 
                    Unif.norm_logical_disagreements ~mode b1 args_lhs args_rhs in
                  if List.length args_lhs = List.length args_rhs then 
                    decompose_and_cont (args_lhs) (args_rhs) rest flag subst
                  else OSeq.empty
                with Unif.Fail -> OSeq.empty
              ) else OSeq.empty
            | _ when different_rigid_heads hd_lhs hd_rhs -> OSeq.empty
            | T.Var _, T.Var _ when P.preunification ->
              aux subst (rest @ [current_constraint])
            | _ -> 
              try
                let mgu =
                  CCList.find_map (fun (name, alg) ->  
                      try
                        let res = alg (lhs, unifscope) (rhs, unifscope) subst in
                        Some (name, res)
                      with 
                      | P.NotInFragment -> 
                        Util.debugf ~section 1 
                          "@[%a@] =?= @[%a@] (subst @[%a@]) is not in fragment of %s" 
                            (fun k -> k T.pp lhs T.pp rhs Subst.pp subst name);
                        None
                      | P.NotUnifiable ->
                        Util.debugf ~section 1 
                          "@[%a@] =?= @[%a@] (subst @[%a@]) is not unif by %s" 
                            (fun k -> k T.pp lhs T.pp rhs Subst.pp subst name);
                        raise Unif.Fail
                    ) (P.frag_algs ()) in 
                match mgu with 
                | Some (name, substs) ->
                  (* We assume that the substitution was augmented so that it is mgu for
                      lhs and rhs *)
                  FList.map (fun sub () -> 
                    Util.debugf ~section 1 
                      "@[%a@] =?= @[%a@] (subst @[%a@]) has unif @[%a@] by %s" 
                        (fun k -> k T.pp lhs T.pp rhs Subst.pp subst Subst.pp sub name);
                    aux sub rest ()) substs
                  |> OSeq.of_list
                  |> OSeq.merge
                | None ->
                  
                  let all_oracles = 
                    P.pb_oracle (body_lhs, unifscope) (body_rhs, unifscope) flag subst unifscope in

                  let res = 
                    all_oracles
                    |> OSeq.map (fun sub_flag_opt ->
                        match sub_flag_opt with 
                        | None -> OSeq.return None
                        | Some (sub', flag') ->
                          try
                            let subst' = Subst.merge subst sub' in
                            incr bind_cnt;
                            delay !bind_cnt (fun () -> aux subst' ((lhs, rhs, flag') :: rest) ())
                          with Subst.InconsistentBinding _ ->
                            OSeq.return None) 
                    |> OSeq.merge
                  in
                  let res = if P.preunification then
                    res else begin
                      if T.is_var hd_lhs && T.is_var hd_rhs && T.equal hd_lhs hd_rhs then begin
                        incr bind_cnt;
                        OSeq.interleave (delay !bind_cnt (fun () -> decompose_and_cont args_lhs args_rhs rest flag subst ())) res 
                      end else res
                    end in
                  if !bind_cnt = 0 && root then (OSeq.cons None res) else res
                  
              with Unif.Fail -> OSeq.empty
            )        
            (* end *) in
    aux ~root:true subst problem

  let try_lfho_unif ((s,_) as t0) ((t,_) as t1) =
    
    (* term is eligible for LFHO unif if it has applied variables
       but has no lambdas -- we want to avoid FO terms to avoid
       returning same unifier twice (since our unif algo will also
       compute FO unifier)  *)
    let eligible_for_lfho t =
      let exception LambdaFound in
      let no_lams t =
        if Iter.exists T.is_fun (T.Seq.subterms ~include_builtin:true t) 
        then raise LambdaFound
        else true in

      let rec aux t = 
        match T.view t with
        | T.App(hd, args) when T.is_const hd -> 
          List.for_all no_lams args
        | T.App(hd, args) -> List.exists aux (hd::args)
        | T.AppBuiltin(_, args)  -> List.exists aux args
        | T.Fun _ -> raise LambdaFound
        (* unif algo can easily take care of naked vars and consts *)
        | _ -> false in
      
      try
        aux t
      with LambdaFound -> false in
    
    if Flex_state.get_exn PUP.k_try_lfho P.flex_state &&
       eligible_for_lfho s && eligible_for_lfho t then (
        try
          OSeq.return (Some (Unif_subst.subst (Unif.FO.unify_full t0 t1)))
        with Unif.Fail -> OSeq.empty
    ) else OSeq.empty

  let unify_scoped t0s t1s =
    
    let lhs,rhs,unifscope,subst = P.identify_scope t0s t1s in

    let bind_cnt = ref 0 in (* number of created binders *)
    let hits_cnt = ref 0 in (* number of unifiers found *)
    try
      OSeq.append 
        ((try_lfho_unif t0s t1s) |> OSeq.map (CCOpt.map Unif_subst.of_subst))
        (do_unif ~bind_cnt ~hits_cnt [(lhs, rhs, P.init_flag)] subst unifscope)
      |> OSeq.map (CCOpt.map (fun subst ->

        

        let renaming = S.Renaming.create () in
        let subst' = Unif_subst.subst subst in
        let cstr = Unif_subst.constr_l subst in
        let _cstr = Unif_constr.apply_subst_l renaming subst' cstr in
        
        

        let norm t = T.normalize_bools @@ Lambda.eta_expand @@ Lambda.snf t in
        let l = norm @@ S.FO.apply renaming subst' t0s in 
        let r = norm @@ S.FO.apply renaming subst' t1s in

        if not (Type.equal (Term.ty l) (Term.ty r)) then (
          assert (false);
        );
          
        if not P.preunification && not ((T.equal l r) && (Type.equal (Term.ty l) (Term.ty r))) then (
          CCFormat.printf "subst:@[%a@]@." Subst.pp subst';
          CCFormat.printf "orig:@[%a@]@.=?=@.@[%a@]@." (Scoped.pp T.pp) t0s (Scoped.pp T.pp) t1s;
          CCFormat.printf "new:@[%a:%a@]@.=?=@.@[%a:%a@]@." T.pp l Type.pp (T.ty l) T.pp r Type.pp (T.ty r);
          assert(false)
        ); subst))
    with Unif.Fail -> OSeq.empty

  let unify_scoped_l t0s t1s =
    let lhs,rhs,unifscope,subst = P.identify_scope_l t0s t1s in
    let problem = FList.map (fun (a,b) -> (a, b, P.init_flag)) (CCList.combine lhs rhs) in 

    let bind_cnt = ref 0 in (* number of created binders *)
    let hits_cnt = ref 0 in (* number of unifiers found *)
    try
      do_unif ~bind_cnt ~hits_cnt problem subst unifscope
    with Unif.Fail -> OSeq.empty
end
