type t = Range of Int32.t * Int32.t
val universe : t
val bottom : t
val singleton : Int32.t -> t
val of_bounds : Int32.t * Int32.t -> t
val contains : t -> t -> bool
val join : t -> t -> t
val neg : t -> t
val check_add : Int32.t -> Int32.t -> bool
val is_safe_add : t -> t -> bool
val add : t -> t -> t
val check_sub : Int32.t -> Int32.t -> bool
val is_safe_sub : t -> t -> bool
val sub : t -> t -> t
val is_safe_mul : 'a -> 'b -> bool
val min : Int32.t list -> Int32.t
val max : Int32.t list -> Int32.t
val mul : t -> t -> t
val is_safe_div : 'a -> t -> bool
val div : t -> t -> t
val is_safe_mod : 'a -> 'b -> bool
val modulo : 'a -> 'b -> 'c
val implies : t * Simple.cmp * Int32.t -> bool
val guard : UnrelState.bop -> t -> t -> t
val widen : t -> t -> t
val to_string : t -> string
