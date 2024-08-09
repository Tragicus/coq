type !+'a t
val empty : 'a t
val find : Constr.t -> 'a t -> 'a
val add : Constr.t -> 'a -> 'a t -> 'a t
