open Names
open Context
open Constr

(* Associative map from Constr.t to T.t. This is implemented using a trie, search end edition is in time linear in the size of the term. To achieve this, given a subterm we are trying to match during the search, when there may be a solution for any term we do not explore the subterm to see if it matches a more precise pattern and backtrack if it does not. Instead, we skip to the next subterm. *)

(* Head pattern of terms *)
module Pat = struct
   (* App takes an int because I need to check the shape of terms, for instance in order to distinguish between `x y z` and `x (y z)` *)
   type t =
      | Any
      | Rel of int
      | Var of Id.t
      | Meta of metavariable
      | Evar of Evar.t
      | Sort
      | Cast
      | Prod of (Name.t, unit) pbinder_annot
      | Lambda of (Name.t, unit) pbinder_annot
      | LetIn of (Name.t, unit) pbinder_annot
      | App of int
      | Const of Constant.t
      | Ind of inductive
      | Construct of constructor
      | Case (* TODO *)
      | Fix (* TODO *)
      | CoFix (* TODO *)
      | Proj of Projection.t
      | Int of Uint63.t
      | Float of Float64.t
      | String of Pstring.t
      | Array of int

   let compare : t -> t -> int = Stdlib.compare

   (* Cuts a term into its head pattern and subterms *)
   let of_constr x : t * Constr.t array =
      match kind x with
      | Constr.Rel i -> (Rel i, [||])
      | Constr.Var i -> (Var i, [||])
      | Constr.Meta i -> (Meta i, [||])
      | Constr.Evar (k, _) -> (Evar k, [||])
      | Constr.Sort _ -> (Sort, [||])
      | Constr.Cast _ -> (Cast, [||])
      | Constr.Prod (a, t, r) -> (Prod { a with binder_relevance = () }, [|t; r|])
      | Constr.Lambda (a, t, r) -> (Lambda { a with binder_relevance = () }, [|t; r|])
      | Constr.LetIn (a, x, _, b) -> (LetIn { a with binder_relevance = () }, [|x; b|])
      | Constr.App (_, args) -> (App (Array.length args), args)
      | Constr.Const (x, _) -> (Const x, [||])
      | Constr.Ind (i, _) -> (Ind i, [||])
      | Constr.Construct (c, _) -> (Construct c, [||])
      | Constr.Case _ -> (Case, [||])
      | Constr.Fix _ -> (Fix, [||])
      | Constr.CoFix _ -> (CoFix, [||])
      | Constr.Proj (p, _, _) -> (Proj p, [||])
      | Constr.Int i -> (Int i, [||])
      | Constr.Float f -> (Float f, [||])
      | Constr.String s -> (String s, [||])
      | Constr.Array (_, a, _, _) -> (Array (Array.length a), a)

end

module PatMap = Map.Make(Pat)

type !+'a t = T of 'a t PatMap.t * 'a option

let empty = T (PatMap.empty, None)

let find k s =
   let rec aux s args =
      let T (m, x) = s in
      try
         let k = Stack.pop args in
         let s =
            try PatMap.find Pat.Any m with Not_found ->
            let (p, kargs) = Pat.of_constr k in
            let kargs = List.rev (Array.to_list kargs) in
            let () = List.iter (fun x -> Stack.push x args) kargs in
            PatMap.find p m
         in aux s args
      with Stack.Empty ->
         (match x with
         | None -> failwith "Reached unreachable code in ConstrMap.find"
         | Some x -> x)
   in let args = Stack.create () in
   let () = Stack.push k args in
   aux s args

let add k x s =
   let rec aux s args =
      let T (m, y) = s in
      try
         let k = Stack.pop args in
         if PatMap.mem Pat.Any m then T (PatMap.add Pat.Any (aux (PatMap.find Pat.Any m) args) m, y) else
         let (p, kargs) = Pat.of_constr k in
         let kargs = List.rev (Array.to_list kargs) in
         let () = List.iter (fun x -> Stack.push x args) kargs in
         let s = try PatMap.find p m with Not_found -> empty in
         T (PatMap.add p (aux s args) m, y)
      with
      | Stack.Empty -> T (m, Some x)
   in let args = Stack.create () in
   let () = Stack.push k args in
   aux s args

let bindings s =
