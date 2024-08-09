open Constr

module type RawType = sig
   type t
end

module Make(T : RawType) = struct
   module Pat = struct
      type t =
         | Rel of int
         | Var of Id.t
         | Meta of metavariable
         | Evar of existential_key
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
         | String of Pstrinq.t
         | Array of int

      let compare = compare

      let of_constr x =
         match kind x with
         | Constr.Rel i -> Rel i
         | Constr.Var i -> Var i
         | Constr.Meta i -> Meta i
         | Constr.Evar (k, _) -> Evar k
         | Constr.Sort _ -> Sort
         | Constr.Cast _ -> Cast
         | Constr.Prod (a, _, _) -> Prod a
         | Constr.Lambda (a, _, _) -> Lambda a
         | Constr.LetIn (a, _, _, _) -> LetIn a
         | Constr.App (_, args) -> App (Array.length arg)
         | Constr.Const (x, _) -> Const x
         | Constr.Ind (i, _) -> Ind i
         | Constr.Construct (c, _) -> Construct c
         | Constr.Case _ -> Case
         | Constr.Fix _ -> Fix
         | Constr.CoFix _ -> CoFix
         | Constr.Proj (p, _, _) -> Proj p
         | Constr.Int i -> Int i
         | Constr.Float f -> Float f
         | Constr.String s -> String s
         | Constr.Array (_, a, _, _) -> Array (Array.length a)

   end

   module PatMap = Map.Make(Pat)

   type t = t PatMap.t * T.t option
end
