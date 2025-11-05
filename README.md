# Monadise

### The problem

Small library attempting to help in the following situations:

- Oh no! I want to map on a list but my function returns an option, what shall I do? 🙁
- Oh noo! My code uses the result monad everywhere, so `Array.fold_left` won't work. 😢
- Oh nooo! I love working with Lwt but now I have comparison functions that return a promise and I cannot sort things anymore. 😭

### A solution

Enter Monadise:

``` ocaml
include Monadise.Make(Option)
monadise_1_1 List.map (fun x -> Some (x + 1)) [1; 2; 3]
(* => Some [2; 3; 4] *)

include Monadise.Make(Result)
monadise_2_2 Array.fold_left (fun acc x -> Ok (acc + x)) 0 [|1; 2; 3|]
(* => Ok 6 *)

include Monadise.Make(Lwt)
monadise_2_1 List.sort (fun x y -> Lwt.return (Int.compare x y)) [3; 1; 2]
(* => a fulfilled promise holding [1; 2; 3] *)
```

### Note on `bind'`

There is however a caveat: the `Option`, `Result` and `Lwt` modules above must have the following signature:

``` ocaml
type 'a t
val return : 'a -> 'a t
val bind' : on_error: (unit -> unit) -> 'a t -> ('a -> 'b t) -> 'b t
```

Note in particular the extra [~on_error] argument to [bind']. This is necessary for garbage collection of continuations. If anyone effect-savvy had an idea of how to get rid of this, I'm all ears.

### How to choose `monadise_<n>_<m>`

The result of `Make` is a module containing:

``` ocaml
val monadise : (('a -> 'b) -> 'c) -> (('a > 'b m) -> 'c m)
```

from which everything else can be derived. However, because this requires some gymnastics and makes things harder to read, a few variants are provided. First, the action (`'a -> 'b` in the above) may need more arguments, for which we provide the `monadise_<n>` variants. Morally, `monadise` is `monadise_1`. As an example:

``` ocaml
val monadise_3 : (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c) -> (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c m)
```

Second, the function might take extra arguments after the action (eg. 1 for `List.sort`, but 2 for `Array.compare`), for which we provide the `monadise_<n>_<m>` variants, where `<n>` is the same as above. Morally, `monadise_<n>` is `monadise_<n>_0`. For instance:

``` ocaml
val monadise_1_2 : (('a -> 'b) -> 'c1 -> 'c2 -> 'd) -> (('a -> 'b m) -> 'c1 -> 'c2 -> 'd m)
```

One can easily read these numbers in the type signature of the target function, eg.:

``` ocaml
val Seq.iteri : (int -> 'a -> unit) -> 'a Seq.t -> unit
(*               \-------/             \------/      *)
(*                   2                     1         *)

val Array.fold_lefti : ('acc -> int -> 'a -> 'acc) -> 'acc -> 'a Array.t -> 'acc
(*                      \---------------/             \----------------/      *)
(*                              3                              2              *)
```

### Breaking the abstraction

Monadise relies on effects, so types won't have your back to ensure that the effect is handled properly. It is therefore important to understand the limitations. Failing to do so will result in an unhandled `Monadise_yield` effect.

There is just one rule: all the calls to the monadic action must take place in the context of the call to `monadise_<n>_<m>`. One example where this rule is easily broken is in `Seq`, for instance, in any function that returns a sequence:

``` ocaml
include Monadise.Make(Option)
let s = monadise_1_1 Seq.map (fun x -> Some (x + 1)) (List.to_seq [1; 2; 3])
(* s has type int Seq.t option *)
let s = Option.get s
(* s is an int Seq.t, but evaluating it runs the action above, and so: *)
let () = Seq.iter (Format.printf " %d") s
(* => Exception: Stdlib.Effect.Unhandled(Monadise_yield(1)) *)
```

In general, Monadise should not be used on any function that stores its action for later use. Not all functions from `Seq` are unsafe, and Monadise will work fine if called directly on `Seq.iter`, for instance, so it would be possible to do:

``` ocaml
include Monadise.Make(Option)
let s = Seq.map (fun x -> Some (x + 1)) (List.to_seq [1; 2; 3])
(* s has type int option Seq.t *)
let _ = monadise_1_1 Seq.iter (Option.map @@ Format.printf " %d") s
(* => prints " 2 3 4" and returns Some () *)
```
