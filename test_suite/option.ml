(* Option is a nullable type which Null is equivalent to nil or null *)
type 'a option =
  | None
  | Some of 'a

(* Unwraps a option *)
let unwrap = function
  | Some x -> x
  | None -> raise (Invalid_argument "unwrap")