(* Leibnz equality. It does perform an important role in
   dependent type propositions. Example of using it to unify
   by reflection/unification:

   ```
   let f : eq 10 10 = Refl
   ```*)
type {'t} ('a : 't) ('b : 't) eq =
  | Refl : eq 'a 'a

(* Identity proof *)
let eq_identity : {'a} -> eq 'a 'a -> eq 'a 'a =
  fun {'a} e -> e

(* Symmetry proof *)
let eq_symmetry : {'a, 'b} -> eq 'a 'b -> eq 'b 'a =
  fun {'a, 'b} -> function
  | Refl -> Refl

(* Transitivity proof *)
let eq_transitivity : {'a, 'b, 'c} -> eq 'a 'b -> eq 'b 'c -> eq 'a 'c =
  fun {'a, 'b, 'c} e e' ->
    match e, e' with
    | Refl, Refl -> Refl

(* Congruence proof *)
let eq_congruence : {'a, 'b, 'f : {'dom, 'cod} -> 'dom -> 'cod} -> eq 'a 'b -> eq ('f 'a) ('f 'b) =
  fun {'a, 'b, 'f} -> function
  | Refl -> Refl

(* Using syntax sugar of `:=` *)
let (:=) e e' = eq e e'

(* Identity proof *)
let identity : {'a} -> ('a := 'a) -> ('a := 'a) =
  fun {'a} e -> e

(* Symmetry proof *)
let symmetry : {'a, 'b} -> ('a := 'b) -> ('b := 'a) =
  fun {'a, 'b} -> function
  | Refl -> Refl

(* Transitivity proof *)
let transitivity : {'a, 'b, 'c} -> ('a := 'b) -> ('b := 'c) -> ('a := 'c) =
  fun {'a, 'b, 'c} e e' ->
    match e, e' with
    | Refl, Refl -> Refl

(* Congruence proof *)
let congruence : {'a, 'b, 'f : ({'dom, 'cod} -> 'dom -> 'cod)} -> ('a := 'b) -> ('f 'a := 'f 'b) =
  fun {'a, 'b, 'f} -> function
  | Refl -> Refl
