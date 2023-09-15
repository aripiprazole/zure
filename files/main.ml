open Batata
open IO

type 'a option = None | Some of 'a
type 'a list = Nil | Cons of 'a * 'a list
type {'t} 'a 'b Eq = Refl : Eq 'a 'a
val f : int -> int
let f x = x + 1
let g x = f x * f x
let main args =
  let open IO in
  let x = read_int () in
  let y = IO.read_int () in
  println @@ g $ x + y