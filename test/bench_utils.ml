let spf = Format.sprintf
let fpf = Format.fprintf
let ( % ) f g = fun x -> f (g x)

let string_of_words f =
  let f = float (Sys.word_size / 8) *. f in
  let a = abs_float f in
  if a < 1_000. then spf "%dB" (int_of_float f)
  else if a < 1_000_000. then spf "%dKB" (int_of_float (f /. 1_000.))
  else if a < 1_000_000_000. then spf "%.1fMB" (f /. 1_000_000.)
  else spf "%.1fGB" (f /. 1_000_000_000.)

let rec list_dropwhile f = function
  | [] -> []
  | x :: xs when f x -> list_dropwhile f xs
  | xs -> xs

let show_duration t =
  let factors = [60; 60; 24; 365] in
  let names = ["second"; "minute"; "hour"; "day"; "year";] in
  let rec loop t acc = function
    | [] -> List.rev (t :: acc)
    | n :: tl -> loop (t / n) (t mod n :: acc) tl
  in
  if t < 1. then spf "%.4f seconds" t
  else if t < 10. then spf "%.2f seconds" t
  else
    loop (int_of_float t) [] factors
    |> List.combine names
    |> List.rev
    |> list_dropwhile (fun (_, x) -> x = 0)
    |> List.filter (fun (_, x) -> x <> 0)
    |> List.map (fun (n, x) -> spf "%u %s%s" x n (if x <> 1 then "s" else ""))
    |> String.concat " "

let speed n t = float n /. (max t epsilon_float)

let si_prefixes = [
  ("Q", 1_000_000_000_000_000_000_000_000_000_000.);
  ("R", 1_000_000_000_000_000_000_000_000_000.);
  ("Y", 1_000_000_000_000_000_000_000_000.);
  ("Z", 1_000_000_000_000_000_000_000.);
  ("E", 1_000_000_000_000_000_000.);
  ("P", 1_000_000_000_000_000.);
  ("T", 1_000_000_000_000.);
  ("G", 1_000_000_000.);
  ("M", 1_000_000.);
  ("k", 1_000.);
  ("", 1.);
  ("m", 0.001);
  ("u", 0.000_001);
  (* FIXME: unicode mu breaks alignments *)
  ("n", 0.000_000_001);
  ("p", 0.000_000_000_001);
  ("f", 0.000_000_000_000_001);
  ("a", 0.000_000_000_000_000_001);
  ("z", 0.000_000_000_000_000_000_001);
  ("y", 0.000_000_000_000_000_000_000_001);
  ("r", 0.000_000_000_000_000_000_000_000_001);
  ("q", 0.000_000_000_000_000_000_000_000_000_001);
]

let string_of_float_si ~unit f =
  let result =
    List.find_map
      (fun (symbol, prefix) ->
        let f' = f /. prefix in
        if f' >= 1. then
          Some (symbol, f')
        else
          None
      )
      si_prefixes
  in
  match result with
  | Some (symbol, f') -> spf "%.1f%s%s" f' symbol unit
  | None -> spf "0%s" unit

let string_of_float_scientific f =
  let rec aux f n =
    if f >= 1. then
      if n = 0 then spf "%.1f" f
      else spf "%.1fe-%d" f n
    else aux (f *. 10.) (n + 1)
  in
  let aux f =
    assert (f > 0.);
    aux f 0
  in
  match Float.classify_float f with
  | FP_zero -> "0"
  | FP_infinite -> if f > 0. then "inf." else "-inf."
  | FP_nan -> "nan"
  | FP_normal | FP_subnormal -> spf "%s%s" (if f < 0. then "-" else "") (aux (Float.abs f))

let string_of_float_si_or_scientific ~unit f =
  match unit with
  | None -> string_of_float_scientific f
  | Some unit -> string_of_float_si ~unit f

let string_of_float_percent f =
  let f = 100. *. f in
  match Float.classify_float f with
  | FP_zero -> "0"
  | FP_infinite -> if f > 0. then "inf." else "-inf."
  | FP_nan -> "nan"
  | FP_normal | FP_subnormal -> spf "%+.2f%%" f

let pp_table fmt lines =
  let n_columns = Array.length (List.hd lines) in
  assert (List.for_all (fun line -> Array.length line = n_columns) lines);
  let widths = Array.init n_columns (fun i -> List.fold_left (fun acc line -> max acc (String.length line.(i))) 0 lines) in
  let pad n s = String.make (n - String.length s) ' ' ^ s in
  let lines = List.map (Array.mapi (fun i -> pad widths.(i))) lines in
  List.iter (fun line -> Array.iter (fpf fmt "  %s") line; fpf fmt "\n") lines

module Gc_stat_diff_per_run = struct
  (** Variant of {!Gc.stat} with only the measurements that we are interested in,
      but only for one run of the benchmarks. Everything is a float, because we
      divide by the number of runs. *)
  type t = {
    time_elapsed: float; (** Time per run. *)
    allocated_words: float; (** Number of words allocated in total. *)
    major_words: float; (** Number of words allocated in the major heap, including the promoted words. *)
    minor_words: float; (** Number of words allocated in the minor heap. *)
    promoted_words: float; (** Number of words allocated in the minor heap that survived a minor collection and were moved to the major heap. *)
    heap_words: float; (** Total size of the major heap, in words, that is the allocated words that were not garbage collected at the end. *)
    compactions: float; (** Number of heap compactions. *)
    major_collections: float; (** Number of major collection cycles completed. *)
    minor_collections: float; (** Number of minor collections. *)
  }

  (** Difference between two {!Gc.stat} as {!t}. *)
  let compute ~after: (time_after, after) ~before: (time_before, before) ~runs =
    let open Gc in
    let runs = float_of_int runs in
    let major_words = (after.major_words -. before.major_words) /. runs in
    let minor_words = (after.minor_words -. before.minor_words) /. runs in
    let promoted_words = (after.promoted_words -. before.promoted_words) /. runs in
    {
      time_elapsed = (time_after -. time_before) /. runs;
      allocated_words = major_words +. minor_words -. promoted_words;
      major_words;
      minor_words;
      promoted_words;
      heap_words = (float_of_int @@ after.heap_words - before.heap_words) /. runs;
      compactions = (float_of_int @@ after.compactions - before.compactions) /. runs;
      major_collections = (float_of_int @@ after.major_collections - before.major_collections) /. runs;
      minor_collections = (float_of_int @@ after.minor_collections - before.minor_collections) /. runs;
    }

  (** Relative increase between a base and a measured {!t}. *)
  let increase ~base measured = {
    time_elapsed = (measured.time_elapsed -. base.time_elapsed) /. base.time_elapsed;
    allocated_words = (measured.allocated_words -. base.allocated_words) /. base.allocated_words;
    major_words = (measured.major_words -. base.major_words) /. base.major_words;
    minor_words = (measured.minor_words -. base.minor_words) /. base.minor_words;
    promoted_words = (measured.promoted_words -. base.promoted_words) /. base.promoted_words;
    heap_words = (measured.heap_words -. base.heap_words) /. base.heap_words;
    compactions = (measured.compactions -. base.compactions) /. base.compactions;
    major_collections = (measured.major_collections -. base.major_collections) /. base.major_collections;
    minor_collections = (measured.minor_collections -. base.minor_collections) /. base.minor_collections;
  }

  let headers = ["time"; "alloc."; "major"; "minor"; "prom."; "heap"; "comp."; "maj.col."; "min.col."]
  let units = [|Some "s"; Some "B"; Some "B"; Some "B"; Some "B"; Some "B"; None; None; None|]
  let columns s = [s.time_elapsed; s.allocated_words; s.major_words; s.minor_words; s.promoted_words; s.heap_words; s.compactions; s.major_collections; s.minor_collections]

  let pp_table fmt (base, measured) =
    pp_table fmt @@
      List.map Array.of_list [
        ("" :: headers);
        ("baseline" :: List.mapi (fun i -> string_of_float_si_or_scientific ~unit: (units.(i))) (columns base));
        ("measured" :: List.mapi (fun i -> string_of_float_si_or_scientific ~unit: (units.(i))) (columns measured));
        ("increase" :: List.map string_of_float_percent (columns (increase ~base measured)));
      ]
end

let run_one ~count f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  let s0 = Gc.quick_stat () in
  for _ = 1 to count do ignore @@ f () done;
  let s1 = Gc.quick_stat () in
  let t1 = Unix.gettimeofday () in
  Gc_stat_diff_per_run.compute ~before: (t0, s0) ~after: (t1, s1) ~runs: count

let run ~base ~measured =
  Format.eprintf "Benchmarking `\027[1mmonadise\027[0m'.@?";
  let base = base () in
  let measured = measured () in
  Format.eprintf "\n%a@." Gc_stat_diff_per_run.pp_table (base, measured)
