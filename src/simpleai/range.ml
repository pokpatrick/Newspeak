open UnrelState;;
open Int32;;

type t = 
  | Range of Int32.t * Int32.t
;;

let universe = Range (min_int, max_int);;

let bottom = Range (one, zero);;

let singleton i = Range (i, i);;

let of_bounds (l, u) = Range (l, u);;

let contains x y =
  match (x, y) with
    | (Range (a1, b1), Range (a2, b2)) -> 
	(compare a1 a2 <= 0) && (compare b1 b2 >= 0)
;;

let join x y =
  match (x, y) with
    | (Range(a, b), Range(c, d)) ->
	let e = if a < c then a else c in
	let f = if b > d then b else d in
	  Range (e, f)
;;

let neg =
  let neg x = 
    if x = min_int then max_int
    else if x = max_int then min_int
    else neg x
  in function
	Range (a, b) -> Range (neg a, neg b)

  	  
let check_add x y = 
  if compare y zero < 0 then
    compare (add x y) x < 0
  else
    compare (add x y) x >= 0 
;;

let is_safe_add x y = 
  match (x, y) with
    | Range (a, b), Range (c, d) -> (check_add a c) && (check_add b d)
;;

let add x y =
  match (x, y) with
    | (Range(a, b), Range(c, d)) ->
	if is_safe_add x y then
	  Range (add a c, add b d)
	else
	  universe
;;

let check_sub x y = 
  if compare y zero < 0 then
    compare (sub x y) x > 0
  else
    compare (sub x y) x <= 0 
;;

let is_safe_sub x y =
  match (x, y) with
    | (Range (a, b), Range (c, d)) ->
	(check_sub b c) && (check_sub a d)
;;
	    
let sub x y =
  match (x, y) with
    | (Range(a, b), Range(c, d)) ->
	if is_safe_sub x y then
	  Range (sub a d, sub b c)
	else
	  universe
;;

let is_safe_mul _ _ = true;;

let min = 
  let rec min acc = function
    | [] -> acc
    | n::rest -> 
	if compare n acc < 0 then
	  min n rest
	else
	  min acc rest
  in function 
    | [] -> raise (failwith "min : empty set")
    | n::rest -> min n rest
;;
	
let max = 
  let rec max acc = function
    | [] -> acc
    | n::rest -> 
	if compare n acc > 0 then
	  max n rest
	else
	  max acc rest
  in function 
    | [] -> raise (failwith "max : empty set")
    | n::rest -> max n rest
;;
	
let mul x y =
  match (x, y) with
    | (Range(a, b), Range(c, d)) ->
	let e = min [mul a c; mul a d; mul b c; mul b d] in
	let f = max [mul a c; mul a d; mul b c; mul b d] in
	  Range (e, f)
;;

let is_safe_div _ y =
  match y with
    | Range (a, b) -> ((compare a zero) >= 0) && ((compare b zero) <= 0)
;;
	    
let div x y =
  match (x, y) with
    | (Range(a, b), Range(c, d)) -> Range(div a c, div b d)
;;

let is_safe_mod _ _ = false;;

let modulo _ _ = failwith "non";;
	
let implies (Range (a, b), cmp, n) =
  match cmp with
    | Simple.Equals -> (compare a n = 0) && (compare b n = 0)
    | Simple.IsLess -> (compare b n <= 0)
;;
	
let guard op c x =
  match (c, x) with
    | (Range (i, _), Range (a, b)) ->
	match op with
	  | EQ ->
	      if (compare a i <= 0) && (compare i b <= 0) then
		singleton i
	      else
		bottom
	  | LTE ->
	      if compare i a <= 0 then 
		Range (a, b)
	      else 
		Range (i, b)
	  | GTE -> 
	      if compare i b >= 0 then 
		Range (a, b)
	      else
		Range (a, i)
	  | LT ->
	      if compare i a < 0 then
		Range (a, b)
	      else
		Range (succ i, b)
	  | GT ->
	      if compare i b > 0 then 
		Range (a, b)
	      else
		Range (a, pred i)
	  | NEQ ->
	      Range(a, b)
;;
	  
let widen r1 r2 =
  match (r1, r2) with
    | (Range (a, b), Range (c, d)) ->
	let e = if compare a c <= 0 then a else min_int in
	let f = if compare b d >= 0 then b else max_int in
	  Range(e, f)
;;

let to_string = function
    Range (a, b) -> 
      "[" ^ to_string a ^ ", " ^ to_string b ^ "]"
