(* Leibnz equality. It does perform an important role in
   dependent type propositions. Example of using it to unify
   by reflection/unification:

   ```
   let f : eq 10 10 = Refl
   ```*)
type {'t} ('a : 't) ('b : 't) eq :=
  | Refl : eq 'a 'a

(* Identity proof *)
val eq_identity : forall 'a. eq 'a 'a -> eq 'a 'a
let eq_identity :=
  fun {'a} e -> e

(* Symmetry proof *)
val eq_symmetry : forall 'a 'b. eq 'a 'b -> eq 'b 'a
let eq_symmetry :=
  fun {'a, 'b} -> function
  | Refl -> Refl

(* Transitivity proof *)
val eq_transitivity : forall 'a 'b 'c. eq 'a 'b -> eq 'b 'c -> eq 'a 'c
let eq_transitivity :=
  fun {'a, 'b, 'c} e e' ->
    match (e, e') with
    | (Refl, Refl) -> Refl

(* Congruence proof *)
val eq_congruence : forall 'a 'b ('f : forall 'dom 'cod. 'dom -> 'cod). eq 'a 'b -> eq ('f 'a) ('f 'b)
let eq_congruence :=
  fun {'a, 'b, 'f} -> function
  | Refl -> Refl

(* Using syntax sugar of `:=` *)
let (=) e e' = eq e e'

(* Identity proof *)
let identity : forall 'a. 'a = 'a -> 'a = 'a :=
  fun {'a} e -> e

(* Symmetry proof *)
let symmetry : forall 'a 'b. 'a = 'b -> 'b = 'a :=
  fun {'a, 'b} -> function
  | Refl -> Refl

(* Transitivity proof *)
let transitivity : forall 'a 'b 'c. 'a = 'b -> 'b = 'c -> 'a = 'c :=
  fun {'a, 'b, 'c} e e' ->
    match (e, e') with
    | (Refl, Refl) -> Refl

(* Congruence proof *)
let congruence : forall 'a 'b ('f : forall 'dom 'cod. 'dom -> 'cod). 'a = 'b -> 'f 'a = 'f 'b :=
  fun {'a, 'b, 'f} -> function
  | Refl -> Refl
