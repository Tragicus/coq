(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open CErrors
open Util
open Vars
open Names
open Context
open Constrintern
open Impargs
open Pretyping

module NamedDecl = Context.Named.Declaration

(* 2| Variable/Hypothesis/Parameter/Axiom declarations *)

(** Declares a local variable/let, possibly declaring it:
    - as a coercion (is_coe)
    - as a type class instance
    - with implicit arguments (impls)
    - with implicit status for discharge (impl)
    - virtually with named universes *)

let declare_local is_coe ~try_assum_as_instance ~kind body typ univs imps impl name =
  let decl = match body with
    | None ->
      Declare.SectionLocalAssum {typ; impl; univs}
    | Some b ->
      Declare.SectionLocalDef {clearbody = (* TODO *) false; entry = Declare.definition_entry ~univs ~types:typ b} in
  let () = Declare.declare_variable ~name ~kind ~typing_flags:None decl in
  let () = if body = None then Declare.assumption_message name else Declare.definition_message name in
  let r = GlobRef.VarRef name in
  let () = maybe_declare_manual_implicits true r imps in
  let _ = if try_assum_as_instance && Option.is_empty body then
      let env = Global.env () in
      let sigma = Evd.from_env env in
      Classes.declare_instance env sigma None Hints.Local r in
  let () =
    if is_coe = Vernacexpr.AddCoercion then
      ComCoercion.try_add_new_coercion
        r ~local:true ~reversible:false in
  (r, UVars.Instance.empty)

let declare_variable is_coe ~kind typ univs imps impl name =
  declare_local is_coe ~try_assum_as_instance:true ~kind:(Decls.IsAssumption kind) None typ univs imps impl name

let instance_of_univ_entry = function
  | UState.Polymorphic_entry univs -> UVars.UContext.instance univs
  | UState.Monomorphic_entry _ -> UVars.Instance.empty

(** Declares a global axiom/parameter, possibly declaring it:
    - as a coercion
    - as a type class instance
    - with implicit arguments
    - with inlining for functor application
    - with named universes *)

let declare_global is_coe ~try_assum_as_instance ~local ~kind ?user_warns body typ (uentry, ubinders as univs) imps nl name =
  let inl = let open Declaremods in match nl with
    | NoInline -> None
    | DefaultInline -> Some (Flags.get_inline_level())
    | InlineAt i -> Some i
  in
  let decl = match body with
    | None -> Declare.ParameterEntry (Declare.parameter_entry ~univs:(uentry, ubinders) ?inline:inl typ)
    | Some b -> Declare.DefinitionEntry (Declare.definition_entry ~univs ~types:typ b) in
  let kn = Declare.declare_constant ~name ~local ~kind ?user_warns decl in
  let gr = GlobRef.ConstRef kn in
  let () = maybe_declare_manual_implicits false gr imps in
  let () = match body with None -> Declare.assumption_message name | Some _ -> Declare.definition_message name in
  let local = match local with
    | Locality.ImportNeedQualified -> true
    | Locality.ImportDefaultBehavior -> false
  in
  let () = if try_assum_as_instance && Option.is_empty body then
      (* why local when is_modtype? *)
      let env = Global.env () in
      let sigma = Evd.from_env env in
      Classes.declare_instance env sigma None Hints.SuperGlobal gr in
  let () =
    if is_coe = Vernacexpr.AddCoercion then
      ComCoercion.try_add_new_coercion
        gr ~local ~reversible:false in
  let inst = instance_of_univ_entry uentry in
  (gr,inst)

let declare_axiom is_coe ~local ~kind ?user_warns typ univs imps nl name =
  declare_global is_coe ~try_assum_as_instance:false ~local ~kind:(Decls.IsAssumption kind) ?user_warns None typ univs imps nl name

let interp_assumption ~program_mode env sigma impl_env bl c =
  let flags = { Pretyping.all_no_fail_flags with program_mode } in
  let sigma, (impls, ((env_bl, ctx), impls1)) = interp_context_evars ~program_mode ~impl_env env sigma bl in
  let sigma, (ty, impls2) = interp_type_evars_impls ~flags env_bl sigma ~impls c in
  let ty = EConstr.it_mkProd_or_LetIn ty ctx in
  sigma, ty, impls1@impls2

let empty_poly_univ_entry = UState.Polymorphic_entry UVars.UContext.empty, UnivNames.empty_binders
let empty_mono_univ_entry = UState.Monomorphic_entry Univ.ContextSet.empty, UnivNames.empty_binders
let empty_univ_entry poly = if poly then empty_poly_univ_entry else empty_mono_univ_entry

(* When declarations are monomorphic (which is always the case in
   sections, even when universes are treated as polymorphic variables)
   the universe constraints and universe names are declared with the
   first declaration only. *)

let clear_univs scope univ =
  match scope, univ with
  | Locality.Global _, (UState.Polymorphic_entry _, _ as univs) -> univs
  | _, (UState.Monomorphic_entry _, _) -> empty_univ_entry false
  | _, (UState.Polymorphic_entry _, _) -> empty_univ_entry true

let context_subst subst (name,b,t,impl) =
  name, Option.map (replace_vars subst) b, replace_vars subst t, impl

let declare_context ~try_global_assum_as_instance ~scope univs nl ctx =
  let fn i subst d =
    let (name,b,t,(impl,kind,is_coe,impls)) = context_subst subst d in
    let univs = if i = 0 then univs else clear_univs scope univs in
    let refu = match scope with
      | Locality.Discharge -> declare_local is_coe ~try_assum_as_instance:true ~kind b t univs impls impl name
      | Locality.Global local -> declare_global is_coe ~try_assum_as_instance:try_global_assum_as_instance ~local ~kind b t univs impls nl name in
    (name, Constr.mkRef refu) :: subst
  in
  let _ = List.fold_left_i fn 0 [] ctx in
  ()

let declare_assumptions ~scope ~kind ?user_warns univs nl l =
  let _, _ = List.fold_left (fun (subst,univs) ((is_coe,idl),typ,imps) ->
      (* NB: here univs are ignored when scope=Discharge *)
      let typ = replace_vars subst typ in
      let univs,subst' =
        List.fold_left_map (fun univs {CAst.v=id} ->
            let refu = match scope with
              | Locality.Discharge ->
                declare_variable is_coe ~kind typ univs imps Glob_term.Explicit id
              | Locality.Global local ->
                declare_axiom is_coe ~local ~kind ?user_warns typ univs imps nl id
            in
            clear_univs scope univs, (id, Constr.mkRef refu))
          univs idl
      in
      subst'@subst, clear_univs scope univs)
      ([], univs) l
  in
  ()

let error_extra_universe_decl ?loc () =
  user_err ?loc
      Pp.(strbrk "When declaring multiple assumptions in one command, " ++
          strbrk "only the first name is allowed to mention a universe binder " ++
          strbrk "(which will be shared by the whole block).")

let extract_assumption_names = function
  | ({CAst.loc;v=id}, Some _) -> error_extra_universe_decl ?loc ()
  | (id, None) -> id

let process_assumptions_udecls = function
  | (coe, ((id, udecl)::ids, c))::assums ->
    let ids = List.map extract_assumption_names ids in
    let assums = List.map (fun (coe, (idl, c)) -> (coe, (List.map extract_assumption_names idl, c))) assums in
    udecl, (coe,(id::ids,c))::assums
  | (_, ([], _))::_ | [] -> assert false

let error_polymorphic_section_variable ?loc () =
  user_err ?loc (Pp.str "Section variables cannot be polymorphic.")

let process_assumptions_no_udecls l =
  List.map (fun (coe, (ids, c)) ->
      (coe, (List.map (function
                 | ({CAst.loc}, Some _) -> error_polymorphic_section_variable ?loc ()
                 | (id, None) -> id) ids, c))) l

let restrict_assumptions_universes sigma l =
  let uvars = List.fold_left (fun uvars (coe,t,imps) ->
      Univ.Level.Set.union uvars (Vars.universes_of_constr t))
      Univ.Level.Set.empty l
  in
  (* XXX: Using `DeclareDef.prepare_parameter` here directly is not
     possible as we indeed declare several parameters; however,
     restrict_universe_context should be called in a centralized place
     IMO, thus I think we should adapt `prepare_parameter` to handle
     this case too. *)
  Evd.restrict_universe_context sigma uvars

let do_assumptions ~program_mode ~poly ~scope ~kind ?user_warns nl l =
  let open Context.Named.Declaration in
  let env = Global.env () in
  let udecl, l = match scope with
    | Locality.Global import_behavior -> process_assumptions_udecls l
    | Locality.Discharge -> None, process_assumptions_no_udecls l in
  let sigma, udecl = interp_univ_decl_opt env udecl in
  let l =
    if poly then
      (* Separate declarations so that A B : Type puts A and B in different levels. *)
      List.fold_right (fun (is_coe,(idl,c)) acc ->
        List.fold_right (fun id acc ->
          (is_coe, ([id], c)) :: acc) idl acc)
        l []
    else l
  in
  (* We interpret all declarations in the same evar_map, i.e. as a telescope. *)
  let (sigma,_,_),l = List.fold_left_map (fun (sigma,env,ienv) (is_coe,(idl,c)) ->
    let sigma,t,imps = interp_assumption ~program_mode env sigma ienv [] c in
    let r = Retyping.relevance_of_type env sigma t in
    let env =
      EConstr.push_named_context (List.map (fun {CAst.v=id} -> LocalAssum (make_annot id r,t)) idl) env in
    let ienv = List.fold_right (fun {CAst.v=id} ienv ->
      let impls = compute_internalization_data env sigma id Variable t imps in
      Id.Map.add id impls ienv) idl ienv in
      ((sigma,env,ienv),((is_coe,idl),t,imps)))
    (sigma,env,empty_internalization_env) l
  in
  let sigma = solve_remaining_evars all_and_fail_flags env sigma in
  (* The universe constraints come from the whole telescope. *)
  let sigma = Evd.minimize_universes sigma in
  let l = List.map (fun (coe,t,imps) -> (coe,EConstr.to_constr sigma t,imps)) l in
  let sigma = restrict_assumptions_universes sigma l in
  let univs = Evd.check_univ_decl ~poly sigma udecl in
  declare_assumptions ~scope ~kind ?user_warns univs nl l

let interp_context_gen env sigma l =
  let sigma, (_, ((env, ctx), impls)) =
    Impargs.with_implicit_protection (fun () ->
        let () = Impargs.make_implicit_args false in
        interp_named_context_evars ~program_mode:false env sigma l) ()
  in
  (* Note, we must use the normalized evar from now on! *)
  let ce t = Pretyping.check_evars env sigma t in
  let () = List.iter (fun decl -> NamedDecl.iter_constr ce decl) ctx in
  let sigma, ctx = Evarutil.finalize
      sigma (fun nf -> List.map (NamedDecl.map_constr_het nf) ctx) in
  (* reorder, evar-normalize and add implicit status *)
  let ctx = List.rev_map (fun d ->
      let {binder_name=name}, b, t = NamedDecl.to_tuple d in
      let impl = let open Glob_term in
      let search x = match x.CAst.v with
        | Some (Name id',max) when Id.equal name id' ->
          Some (if max then MaxImplicit else NonMaxImplicit)
        | _ -> None
        in
        Option.default Explicit (CList.find_map search impls)
      in
      let kind = Decls.(if b = None then IsAssumption Context else IsDefinition LetContext) in
      let data = (impl,kind,Vernacexpr.NoCoercion,[]) in
      (name,b,t,data))
      ctx
  in
   sigma, ctx

let do_context ~poly l =
  let sec = Lib.sections_are_opened () in
  if Dumpglob.dump () then begin
    let l = List.map (function
        | Constrexpr.CLocalAssum (l, _, _, _) ->
           let ty = if sec then "var "else "ax" in List.map (fun n -> ty, n) l
        | Constrexpr.CLocalDef (n, _, _, _) ->
           let ty = if sec then "var "else "def" in [ty, n]
        | Constrexpr.CLocalPattern _ -> [])
      l in
    List.iter (function
        | ty, {CAst.v = Names.Name.Anonymous; _} -> ()
        | ty, {CAst.v = Names.Name.Name id; loc} ->
           Dumpglob.dump_definition (CAst.make ?loc id) sec ty)
      (List.flatten l) end;
  let env = Global.env() in
  let sigma = Evd.from_env env in
  let sigma, ctx = interp_context_gen env sigma l in
  let univs = Evd.univ_entry ~poly sigma in
  let open Locality in
  let scope =
    if sec then Discharge
    else Global (if Lib.is_modtype () then ImportDefaultBehavior else ImportNeedQualified)
  in
  declare_context ~try_global_assum_as_instance:true ~scope univs Declaremods.NoInline ctx

(* API compatibility (used in Elpi) *)

let interp_context env sigma ctx =
  let reverse_rel_context_of_reverse_named_context ctx =
    List.rev (snd (List.fold_left_i (fun n (subst, ctx) (id,b,t,impl) ->
        let decl = (id, Option.map (subst_vars subst) b, subst_vars subst t, impl) in
        (id :: subst, decl :: ctx)) 1 ([],[]) ctx)) in
  let sigma, ctx = interp_context_gen env sigma ctx in
  let ctx = List.map (fun (id,b,t,(impl,_,_,_)) -> (id,b,t,impl)) ctx in
  sigma, reverse_rel_context_of_reverse_named_context ctx
