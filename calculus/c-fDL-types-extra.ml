needs "holist.ml";;

let th4_table = Hashtbl.create 15;;
let bij_table = Hashtbl.create 15;;
let defth_table = Hashtbl.create 15;;
let deftharg_table = Hashtbl.create 15;;



let define_type_raw_test =

  (* ----------------------------------------------------------------------- *)
  (* Handy utility to produce "SUC o SUC o SUC ..." form of numeral.         *)
  (* ----------------------------------------------------------------------- *)

  let sucivate =
    let zero = `0` and suc = `SUC` in
    fun n -> funpow n (curry mk_comb suc) zero in

  (* ----------------------------------------------------------------------- *)
  (* Eliminate local "definitions" in hyps.                                  *)
  (* ----------------------------------------------------------------------- *)


  let SCRUB_EQUATION eq (th,insts) = (*HA*)
    let eq' = itlist subst (map (fun t -> [t]) insts) eq in
    let l,r = dest_eq eq' in
    (MP (INST [r,l] (DISCH eq' th)) (REFL r),(r,l)::insts) in

  (* ----------------------------------------------------------------------- *)
  (* Proves existence of model (inductively); use pseudo-constructors.       *)
  (*                                                                         *)
  (* Returns suitable definitions of constructors in terms of CONSTR, and    *)
  (* the rule and induction theorems from the inductive relation package.    *)
  (* ----------------------------------------------------------------------- *)

  let justify_inductive_type_model =
    let t_tm = `T` and n_tm = `n:num` and beps_tm = `@x:bool. T` in
    let rec munion s1 s2 =
      if s1 = [] then s2 else
      let h1 = hd s1
      and s1' = tl s1 in
      try let _,s2' = remove (fun h2 -> h2 = h1) s2 in h1::(munion s1' s2')
      with Failure _ -> h1::(munion s1' s2) in
    fun def ->
      let newtys,rights = unzip def in
      let tyargls = itlist ((@) o map snd) rights [] in
      let alltys = itlist (munion o C subtract newtys) tyargls [] in
      let epstms = map (fun ty -> mk_select(mk_var("v",ty),t_tm)) alltys in
      let pty =
        try end_itlist (fun ty1 ty2 -> mk_type("prod",[ty1;ty2])) alltys
        with Failure _ -> bool_ty in
      let recty = mk_type("recspace",[pty]) in
      let constr = mk_const("CONSTR",[pty,aty]) in
      let fcons = mk_const("FCONS",[recty,aty]) in
      let bot = mk_const("BOTTOM",[pty,aty]) in
      let bottail = mk_abs(n_tm,bot) in
      let mk_constructor n (cname,cargs) =
        let ttys = map (fun ty -> if mem ty newtys then recty else ty) cargs in
        let args = make_args "a" [] ttys in
        let rargs,iargs = partition (fun t -> type_of t = recty) args in
        let rec mk_injector epstms alltys iargs =
          if alltys = [] then [] else
          let ty = hd alltys in
          try let a,iargs' = remove (fun t -> type_of t = ty) iargs in
              a::(mk_injector (tl epstms) (tl alltys) iargs')
          with Failure _ ->
              (hd epstms)::(mk_injector (tl epstms) (tl alltys) iargs) in
        let iarg =
          try end_itlist (curry mk_pair) (mk_injector epstms alltys iargs)
          with Failure _ -> beps_tm in
        let rarg = itlist (mk_binop fcons) rargs bottail in
      let conty = itlist mk_fun_ty (map type_of args) recty in
      let condef = list_mk_comb(constr,[sucivate n; iarg; rarg]) in
      mk_eq(mk_var(cname,conty),list_mk_abs(args,condef)) in
    let rec mk_constructors n rights =
      if rights = [] then [] else
      (mk_constructor n (hd rights))::(mk_constructors (n + 1) (tl rights)) in
    let condefs = mk_constructors 0 (itlist (@) rights []) in
    let conths = map ASSUME condefs in
    let predty = mk_fun_ty recty bool_ty in
    let edefs = itlist (fun (x,l) acc -> map (fun t -> x,t) l @ acc) def [] in
    let idefs = map2 (fun (r,(_,atys)) def -> (r,atys),def) edefs condefs in
    let mk_rule ((r,a),condef) =
      let left,right = dest_eq condef in
      let args,bod = strip_abs right in
      let lapp = list_mk_comb(left,args) in
      let conds = itlist2
        (fun arg argty sofar ->
          if mem argty newtys then
            mk_comb(mk_var(dest_vartype argty,predty),arg)::sofar
          else sofar) args a [] in
      let conc = mk_comb(mk_var(dest_vartype r,predty),lapp) in
      let rule = if conds = [] then conc
                 else mk_imp(list_mk_conj conds,conc) in
      list_mk_forall(args,rule) in
    let rules = list_mk_conj (map mk_rule idefs) in
    let th0 = derive_nonschematic_inductive_relations rules in
    let th1 = prove_monotonicity_hyps th0 in
    let th2a,th2bc = CONJ_PAIR th1 in
    let th2b = CONJUNCT1 th2bc in
    conths,th2a,th2b in

  (* ----------------------------------------------------------------------- *)
  (* Shows that the predicates defined by the rules are all nonempty.        *)
  (* (This could be done much more efficiently/cleverly, but it's OK.)       *)
  (* ----------------------------------------------------------------------- *)

  let prove_model_inhabitation rth =
    let srules = map SPEC_ALL (CONJUNCTS rth) in
    let imps,bases = partition (is_imp o concl) srules in
    let concs = map concl bases @ map (rand o concl) imps in
    let preds = setify (map (repeat rator) concs) in
    let rec exhaust_inhabitations ths sofar =
      let dunnit = setify(map (fst o strip_comb o concl) sofar) in
      let useful = filter
        (fun th -> not (mem (fst(strip_comb(rand(concl th)))) dunnit)) ths in
      if useful = [] then sofar else
      let follow_horn thm =
        let preds = map (fst o strip_comb) (conjuncts(lhand(concl thm))) in
        let asms = map
          (fun p -> find (fun th -> fst(strip_comb(concl th)) = p) sofar)
          preds in
        MATCH_MP thm (end_itlist CONJ asms) in
      let newth = tryfind follow_horn useful in
      exhaust_inhabitations ths (newth::sofar) in
    let ithms = exhaust_inhabitations imps bases in
    let exths = map
      (fun p -> find (fun th -> fst(strip_comb(concl th)) = p) ithms) preds in
    exths in

  (* ----------------------------------------------------------------------- *)
  (* Makes a type definition for one of the defined subsets.                 *)
  (* ----------------------------------------------------------------------- *)

  let define_inductive_type cdefs exth =
    let extm = concl exth in
    let epred = fst(strip_comb extm) in
    let ename = fst(dest_var epred) in
    let th1 = ASSUME (find (fun eq -> lhand eq = epred) (hyp exth)) in
    let th2 = TRANS th1 (SUBS_CONV cdefs (rand(concl th1))) in
    let th3 = EQ_MP (AP_THM th2 (rand extm)) exth in
    let th4,_ = itlist SCRUB_EQUATION (hyp th3) (th3,[]) in
    let mkname = "_mk_"^ename and destname = "_dest_"^ename in
    let bij1,bij2 = new_basic_type_definition ename (mkname,destname) th4 in
    Hashtbl.add th4_table ename th4;
    Hashtbl.add bij_table ename (CONJ (GEN_ALL bij1)(GEN_ALL (CONV_RULE(LAND_CONV (TRY_CONV BETA_CONV)) bij2)));
    let bij2a = AP_THM th2 (rand(rand(concl bij2))) in
    let bij2b = TRANS bij2a bij2 in
    bij1,bij2b in

  (* ----------------------------------------------------------------------- *)
  (* Defines a type constructor corresponding to current pseudo-constructor. *)
  (* ----------------------------------------------------------------------- *)

  let define_inductive_type_constructor defs consindex th =
    let avs,bod = strip_forall(concl th) in
    let asms,conc =
      if is_imp bod then conjuncts(lhand bod),rand bod else [],bod in
    let asmlist = map dest_comb asms in
    let cpred,cterm = dest_comb conc in
    let oldcon,oldargs = strip_comb cterm in
    let modify_arg v =
      try let dest = snd(assoc (rev_assoc v asmlist) consindex) in
          let ty' = hd(snd(dest_type(type_of dest))) in
          let v' = mk_var(fst(dest_var v),ty') in
          mk_comb(dest,v'),v'
      with Failure _ -> v,v in
    let newrights,newargs = unzip(map modify_arg oldargs) in
    let retmk = fst(assoc cpred consindex) in
    let defbod = mk_comb(retmk,list_mk_comb(oldcon,newrights)) in
    let defrt = list_mk_abs(newargs,defbod) in
    let expth = find (fun th -> lhand(concl th) = oldcon) defs in
    let rexpth = SUBS_CONV [expth] defrt in
    let deflf = mk_var(fst(dest_var oldcon),type_of defrt) in
    let defth = new_definition(mk_eq(deflf,rand(concl rexpth))) in
    Hashtbl.add deftharg_table (fst(dest_var oldcon)) (mk_eq(deflf,rand(concl rexpth)));
    Hashtbl.add defth_table (fst(dest_var oldcon)) defth;
    TRANS defth (SYM rexpth) in

  (* ----------------------------------------------------------------------- *)
  (* Instantiate the induction theorem on the representatives to transfer    *)
  (* it to the new type(s). Uses "\x. rep-pred(x) /\ P(mk x)" for "P".       *)
  (* ----------------------------------------------------------------------- *)

  let instantiate_induction_theorem consindex ith =
    let avs,bod = strip_forall(concl ith) in
    let corlist = map((repeat rator F_F repeat rator) o dest_imp o body o rand)
      (conjuncts(rand bod)) in
    let consindex' = map (fun v -> let w = rev_assoc v corlist in
                                   w,assoc w consindex) avs in
    let recty = (hd o snd o dest_type o type_of o fst o snd o hd) consindex in
    let newtys = map (hd o snd o dest_type o type_of o snd o snd) consindex' in
    let ptypes = map (C mk_fun_ty bool_ty) newtys in
    let preds = make_args "P" [] ptypes in
    let args = make_args "x" [] (map (K recty) preds) in
    let lambs = map2 (fun (r,(m,d)) (p,a) ->
                       mk_abs(a,mk_conj(mk_comb(r,a),mk_comb(p,mk_comb(m,a)))))
                     consindex' (zip preds args) in
    SPECL lambs ith in

  (* ----------------------------------------------------------------------- *)
  (* Reduce a single clause of the postulated induction theorem (old_ver) ba *)
  (* to the kind wanted for the new type (new_ver); |- new_ver ==> old_ver   *)
  (* ----------------------------------------------------------------------- *)

  let pullback_induction_clause tybijpairs conthms =
    let PRERULE = GEN_REWRITE_RULE (funpow 3 RAND_CONV) (map SYM conthms) in
    let IPRULE = SYM o GEN_REWRITE_RULE I (map snd tybijpairs) in
    fun rthm tm ->
      let avs,bimp = strip_forall tm in
      if is_imp bimp then
        let ant,con = dest_imp bimp in
        let ths = map (CONV_RULE BETA_CONV) (CONJUNCTS (ASSUME ant)) in
        let tths,pths = unzip (map CONJ_PAIR ths) in
        let tth = MATCH_MP (SPEC_ALL rthm) (end_itlist CONJ tths) in
        let mths = map IPRULE (tth::tths) in
        let conth1 = BETA_CONV con in
        let contm1 = rand(concl conth1) in
        let conth2 = TRANS conth1
          (AP_TERM (rator contm1) (SUBS_CONV (tl mths) (rand contm1))) in
        let conth3 = PRERULE conth2 in
        let lctms = map concl pths in
        let asmin = mk_imp(list_mk_conj lctms,rand(rand(concl conth3))) in
        let argsin = map rand (conjuncts(lhand asmin)) in
        let argsgen =
          map (fun tm -> mk_var(fst(dest_var(rand tm)),type_of tm)) argsin in
        let asmgen = subst (zip argsgen argsin) asmin in
        let asmquant =
          list_mk_forall(snd(strip_comb(rand(rand asmgen))),asmgen) in
        let th1 = INST (zip argsin argsgen) (SPEC_ALL (ASSUME asmquant)) in
        let th2 = MP th1 (end_itlist CONJ pths) in
        let th3 = EQ_MP (SYM conth3) (CONJ tth th2) in
        DISCH asmquant (GENL avs (DISCH ant th3))
      else
        let con = bimp in
        let conth2 = BETA_CONV con in
        let tth = PART_MATCH I rthm (lhand(rand(concl conth2))) in
        let conth3 = PRERULE conth2 in
        let asmgen = rand(rand(concl conth3)) in
        let asmquant = list_mk_forall(snd(strip_comb(rand asmgen)),asmgen) in
        let th2 = SPEC_ALL (ASSUME asmquant) in
        let th3 = EQ_MP (SYM conth3) (CONJ tth th2) in
        DISCH asmquant (GENL avs th3) in

  (* ----------------------------------------------------------------------- *)
  (* Finish off a consequence of the induction theorem.                      *)
  (* ----------------------------------------------------------------------- *)

  let finish_induction_conclusion consindex tybijpairs =
    let tybij1,tybij2 = unzip tybijpairs in
    let PRERULE =
      GEN_REWRITE_RULE (LAND_CONV o LAND_CONV o RAND_CONV) tybij1 o
      GEN_REWRITE_RULE LAND_CONV tybij2
    and FINRULE = GEN_REWRITE_RULE RAND_CONV tybij1 in
    fun th ->
      let av,bimp = dest_forall(concl th) in
      let pv = lhand(body(rator(rand bimp))) in
      let p,v = dest_comb pv in
      let mk,dest = assoc p consindex in
      let ty = hd(snd(dest_type(type_of dest))) in
      let v' = mk_var(fst(dest_var v),ty) in
      let dv = mk_comb(dest,v') in
      let th1 = PRERULE (SPEC dv th) in
      let th2 = MP th1 (REFL (rand(lhand(concl th1)))) in
      let th3 = CONV_RULE BETA_CONV th2 in
      GEN v' (FINRULE (CONJUNCT2 th3)) in

  (* ----------------------------------------------------------------------- *)
  (* Derive the induction theorem.                                           *)
  (* ----------------------------------------------------------------------- *)

  let derive_induction_theorem consindex tybijpairs conthms iith rth =
    let bths = map2
      (pullback_induction_clause tybijpairs conthms)
      (CONJUNCTS rth) (conjuncts(lhand(concl iith))) in
    let asm = list_mk_conj(map (lhand o concl) bths) in
    let ths = map2 MP bths (CONJUNCTS (ASSUME asm)) in
    let th1 = MP iith (end_itlist CONJ ths) in
    let th2 = end_itlist CONJ (map
      (finish_induction_conclusion consindex tybijpairs) (CONJUNCTS th1)) in
    let th3 = DISCH asm th2 in
    let preds = map (rator o body o rand) (conjuncts(rand(concl th3))) in
    let th4 = GENL preds th3 in
    let pasms = filter (C mem (map fst consindex) o lhand) (hyp th4) in
    let th5 = itlist DISCH pasms th4 in
    let th6,_ = itlist SCRUB_EQUATION (hyp th5) (th5,[]) in
    let th7 = UNDISCH_ALL th6 in
    fst (itlist SCRUB_EQUATION (hyp th7) (th7,[])) in

  (* ----------------------------------------------------------------------- *)
  (* Create the recursive functions and eliminate pseudo-constructors.       *)
  (* (These are kept just long enough to derive the key property.)           *)
  (* ----------------------------------------------------------------------- *)

  let create_recursive_functions tybijpairs consindex conthms rth =
    let domtys = map (hd o snd o dest_type o type_of o snd o snd) consindex in
    let recty = (hd o snd o dest_type o type_of o fst o snd o hd) consindex in
    let ranty = mk_vartype "Z" in
    let fn = mk_var("fn",mk_fun_ty recty ranty)
    and fns = make_args "fn" [] (map (C mk_fun_ty ranty) domtys) in
    let args = make_args "a" [] domtys in
    let rights = map2 (fun (_,(_,d)) a -> mk_abs(a,mk_comb(fn,mk_comb(d,a))))
      consindex args in
    let eqs = map2 (curry mk_eq) fns rights in
    let fdefs = map ASSUME eqs in
    let fxths1 = map (fun th1 -> tryfind (fun th2 -> MK_COMB(th2,th1)) fdefs)
      conthms in
    let fxths2 = map (fun th -> TRANS th (BETA_CONV (rand(concl th)))) fxths1 in
    let mk_tybijcons (th1,th2) =
      let th3 = INST [rand(lhand(concl th1)),rand(lhand(concl th2))] th2 in
      let th4 = AP_TERM (rator(lhand(rand(concl th2)))) th1 in
      EQ_MP (SYM th3) th4 in
    let SCONV = GEN_REWRITE_CONV I (map mk_tybijcons tybijpairs)
    and ERULE = GEN_REWRITE_RULE I (map snd tybijpairs) in
    let simplify_fxthm rthm fxth =
      let pat = funpow 4 rand (concl fxth) in
      if is_imp(repeat (snd o dest_forall) (concl rthm)) then
        let th1 = PART_MATCH (rand o rand) rthm pat in
        let tms1 = conjuncts(lhand(concl th1)) in
        let ths2 = map (fun t -> EQ_MP (SYM(SCONV t)) TRUTH) tms1 in
        ERULE (MP th1 (end_itlist CONJ ths2))
      else
        ERULE (PART_MATCH rand rthm pat) in
    let fxths3 = map2 simplify_fxthm (CONJUNCTS rth) fxths2 in
    let fxths4 = map2 (fun th1 -> TRANS th1 o AP_TERM fn) fxths2 fxths3 in
    let cleanup_fxthm cth fxth =
      let tms = snd(strip_comb(rand(rand(concl fxth)))) in
      let kth = RIGHT_BETAS tms (ASSUME (hd(hyp cth))) in
      TRANS fxth (AP_TERM fn kth) in
    let fxth5 = end_itlist CONJ (map2 cleanup_fxthm conthms fxths4) in
    let pasms = filter (C mem (map fst consindex) o lhand) (hyp fxth5) in
    let fxth6 = itlist DISCH pasms fxth5 in
    let fxth7,_ =
      itlist SCRUB_EQUATION (itlist (union o hyp) conthms []) (fxth6,[]) in
    let fxth8 = UNDISCH_ALL fxth7 in
    fst (itlist SCRUB_EQUATION (subtract (hyp fxth8) eqs) (fxth8,[])) in

  (* ----------------------------------------------------------------------- *)
  (* Create a function for recursion clause.                                 *)
  (* ----------------------------------------------------------------------- *)

  let create_recursion_iso_constructor =
    let s = `s:num->Z` in
    let zty = `:Z` in
    let numty = `:num` in
    let rec extract_arg tup v =
      if v = tup then REFL tup else
      let t1,t2 = dest_pair tup in
      let PAIR_th = ISPECL [t1;t2] (if free_in v t1 then FST else SND) in
      let tup' = rand(concl PAIR_th) in
      if tup' = v then PAIR_th else
      let th = extract_arg (rand(concl PAIR_th)) v in
      SUBS[SYM PAIR_th] th in
    fun consindex ->
      let recty = hd(snd(dest_type(type_of(fst(hd consindex))))) in
      let domty = hd(snd(dest_type recty)) in
      let i = mk_var("i",domty)
      and r = mk_var("r",mk_fun_ty numty recty) in
      let mks = map (fst o snd) consindex in
      let mkindex = map (fun t -> hd(tl(snd(dest_type(type_of t)))),t) mks in
      fun cth ->
        let artms = snd(strip_comb(rand(rand(concl cth)))) in
        let artys = mapfilter (type_of o rand) artms in
        let args,bod = strip_abs(rand(hd(hyp cth))) in
        let ccitm,rtm = dest_comb bod in
        let cctm,itm = dest_comb ccitm in
        let rargs,iargs = partition (C free_in rtm) args in
        let xths = map (extract_arg itm) iargs in
        let cargs' = map (subst [i,itm] o lhand o concl) xths in
        let indices = map sucivate (0--(length rargs - 1)) in
        let rindexed = map (curry mk_comb r) indices in
        let rargs' = map2 (fun a rx -> mk_comb(assoc a mkindex,rx))
            artys rindexed in
        let sargs' = map (curry mk_comb s) indices in
        let allargs = cargs'@ rargs' @ sargs' in
        let funty = itlist (mk_fun_ty o type_of) allargs zty in
        let funname = fst(dest_const(repeat rator (lhand(concl cth))))^"'" in
        let funarg = mk_var(funname,funty) in
        list_mk_abs([i;r;s],list_mk_comb(funarg,allargs)) in

  (* ----------------------------------------------------------------------- *)
  (* Derive the recursion theorem.                                           *)
  (* ----------------------------------------------------------------------- *)

  let derive_recursion_theorem =
    let CCONV = funpow 3 RATOR_CONV (REPEATC (GEN_REWRITE_CONV I [FCONS])) in
    fun tybijpairs consindex conthms rath ->
      let isocons = map (create_recursion_iso_constructor consindex) conthms in
      let ty = type_of(hd isocons) in
      let fcons = mk_const("FCONS",[ty,aty])
      and fnil = mk_const("FNIL",[ty,aty]) in
      let bigfun = itlist (mk_binop fcons) isocons fnil in
      let eth = ISPEC bigfun CONSTR_REC in
      let fn = rator(rand(hd(conjuncts(concl rath)))) in
      let betm = let v,bod = dest_abs(rand(concl eth)) in vsubst[fn,v] bod in
      let LCONV = REWR_CONV (ASSUME betm) in
      let fnths =
        map (fun t -> RIGHT_BETAS [bndvar(rand t)] (ASSUME t)) (hyp rath) in
      let SIMPER = PURE_REWRITE_RULE
        (map SYM fnths @ map fst tybijpairs @ [FST; SND; FCONS; BETA_THM]) in
      let hackdown_rath th =
        let ltm,rtm = dest_eq(concl th) in
        let wargs = snd(strip_comb(rand ltm)) in
        let th1 = TRANS th (LCONV rtm) in
        let th2 = TRANS th1 (CCONV (rand(concl th1))) in
        let th3 = TRANS th2 (funpow 2 RATOR_CONV BETA_CONV (rand(concl th2))) in
        let th4 = TRANS th3 (RATOR_CONV BETA_CONV (rand(concl th3))) in
        let th5 = TRANS th4 (BETA_CONV (rand(concl th4))) in
        GENL wargs (SIMPER th5) in
      let rthm = end_itlist CONJ (map hackdown_rath (CONJUNCTS rath)) in
      let seqs =
        let unseqs = filter is_eq (hyp rthm) in
        let tys = map (hd o snd o dest_type o type_of o snd o snd) consindex in
        map (fun ty -> find
          (fun t -> hd(snd(dest_type(type_of(lhand t)))) = ty) unseqs) tys in
      let rethm = itlist EXISTS_EQUATION seqs rthm in
      let fethm = CHOOSE(fn,eth) rethm in
      let pcons = map (repeat rator o rand o repeat (snd o dest_forall))
        (conjuncts(concl rthm)) in
      GENL pcons fethm in

  (* ----------------------------------------------------------------------- *)
  (* Basic function: returns induction and recursion separately. No parser.  *)
  (* ----------------------------------------------------------------------- *)

  fun def ->
    let defs,rth,ith = justify_inductive_type_model def in
    let neths = prove_model_inhabitation rth in
    let tybijpairs = map (define_inductive_type defs) neths in
    let preds = map (repeat rator o concl) neths in
    let mkdests = map
      (fun (th,_) -> let tm = lhand(concl th) in rator tm,rator(rand tm))
      tybijpairs in
    let consindex = zip preds mkdests in
    let condefs = map (define_inductive_type_constructor defs consindex)
                      (CONJUNCTS rth) in
    let conthms = map
      (fun th -> let args = fst(strip_abs(rand(concl th))) in
                 RIGHT_BETAS args th) condefs in
    let iith = instantiate_induction_theorem consindex ith in
    let fth = derive_induction_theorem consindex tybijpairs conthms iith rth in
    let rath = create_recursive_functions tybijpairs consindex conthms rth in
    let kth = derive_recursion_theorem tybijpairs consindex conthms rath in
    fth,kth;;


let define_type_test s =
  try let retval = assoc s (!the_inductive_types) in
      (warn true "Benign redefinition of inductive type"; retval)
  with Failure _ ->
      let defspec = parse_inductive_type_specification s in
      let newtypes = map fst defspec
      and constructors = itlist ((@) o map fst) (map snd defspec) [] in
      if not(length(setify newtypes) = length newtypes)
      then failwith "define_type: multiple definitions of a type"
      else if not(length(setify constructors) = length constructors)
      then failwith "define_type: multiple instances of a constructor"
      else if exists (can get_type_arity o dest_vartype) newtypes
      then let t = find (can get_type_arity) (map dest_vartype newtypes) in
           failwith("define_type: type :"^t^" already defined")
      else if exists (can get_const_type) constructors
      then let t = find (can get_const_type) constructors in
           failwith("define_type: constant "^t^" already defined")
      else
        let retval = define_type_raw_test defspec in
        the_inductive_types := (s,retval)::(!the_inductive_types); retval;;

let make_exist_theorem th4 = 
    let blah = mk_var ("a", (type_of (rand (concl th4)))) in
    let EWit = rand (concl th4) in
    let FinalTheorem = ONCE_REWRITE_RULE [BETA_CONV (concl th4)] th4 in
    let ExistVersion = mk_exists (blah, rhs (concl (BETA_CONV (mk_comb ((rator (concl th4)),blah))))) in
    let ExistTheorem = EXISTS (ExistVersion,  EWit) (FinalTheorem) in
    ExistTheorem;;
    
let new_type_definition_stuff ename = 
    let exth = make_exist_theorem (Hashtbl.find th4_table ename) in
    let bij = Hashtbl.find bij_table ename in
    thm_db_print_type_definition ename ("_mk_"^ename) ("_dest_"^ename) exth bij;
    flush_str_formatter();;

 
let new_definition_stuff ename = 
    let deftharg = Hashtbl.find deftharg_table ename in
    let defth = Hashtbl.find defth_table ename in
    thm_db_print_definition true "DRULE" defth deftharg None [ename];
    flush_str_formatter();;
       


             
		



let string_INDUCT, string_RECURSION = define_type_test "string = String num";;


let form_INDUCT, form_RECURSION = define_type_test 
"form = pp pform
	  | nn nform;
pform = pAtom string 
	  | ** form form
	  | /- form form
	  | \- form form
	  | <> form;
nform = nAtom string 
	  | ## form form
	  | \. form form	  
	  | /. form form
	  | || form";;	
      
let struct_INDUCT, struct_RECURSION = define_type_test 
"strF = ff form 
	  | <>^ strF
	  | **^ strF strF
	  | /-^ strF strG
	  | \-^ strG strF
	  | focf form;
 strG = gg form
	  | ||^ strG
	  | ##^ strG strG
	  | /.^ strG strF
	  | \.^ strF strG
	  | focg form";;
let seq_INDUCT, seq_RECURSION = define_type_test 	  
"seq  = --> strF strG";;	




(*
Hashtbl.find_all th1_table "seq";;
hd (Hashtbl.find_all th4_table "form");;
Hashtbl.find_all th4_table "form";;
Hashtbl.find_all th4_table "nform";;
Hashtbl.find_all th4_table "pform";;
Hashtbl.find_all bij_table "string";;

Hashtbl.find_all defth_table "String";;
Hashtbl.find_all deftharg_table "**";;
Hashtbl.length defth_table;;
Hashtbl.length th1_table;;
*)

"new_type_definition:";;
Hashtbl.iter (fun k v -> print_endline k) th4_table;;
new_type_definition_stuff "string";;
new_type_definition_stuff "form";;
new_type_definition_stuff "pform";;
new_type_definition_stuff "nform";;
new_type_definition_stuff "strF";;
new_type_definition_stuff "strG";;
new_type_definition_stuff "seq";;


"new_definition:";;
Hashtbl.iter (fun k v -> print_endline k) defth_table;;
new_definition_stuff "||";;
new_definition_stuff "nn";;
new_definition_stuff "pp";;
new_definition_stuff "/.^";;
new_definition_stuff "ff";;
new_definition_stuff "<>^";;
new_definition_stuff "gg";;
new_definition_stuff "/-^";;
new_definition_stuff "##";;
new_definition_stuff "||^";;
new_definition_stuff "\-^";;
new_definition_stuff "/.";;
new_definition_stuff "/-";;
new_definition_stuff "pAtom";;
new_definition_stuff "String";;
new_definition_stuff "\.^";;
new_definition_stuff "focf";;
new_definition_stuff "**";;
new_definition_stuff "-->";;
new_definition_stuff "##^";;
new_definition_stuff "**^";;
new_definition_stuff "<>";;
new_definition_stuff "\-";;
new_definition_stuff "focg";;
new_definition_stuff "\.";;
new_definition_stuff "nAtom";;

(*
let th4 = hd (Hashtbl.find_all th4_table "string");;
let blah =  `a:(num)recspace`;;
let blah =  `a:((char)list)recspace`;;
let exth = make_exist_theorem th4 blah;;
new_type_definition "string2" ("_mk_string2", "_dest_string2") exth;;
*)