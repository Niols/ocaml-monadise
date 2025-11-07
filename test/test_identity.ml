include Monadise.Make(struct
  type 'a t = 'a
  let return x = x
  let bind' ~on_error: _ x f = f x
end)

open Alcotest

let test_list_iter =
  test_case "monadise_1_1 List.iter" `Quick @@ fun () ->
  let l = [1; 2; 3; 4; 5] in
  let buf = ref [] in
  check'
    unit
    ~msg: "run succeeds"
    ~expected: ()
    ~actual: (monadise_1_1 List.iter (fun x -> buf := x :: !buf) l);
  check'
    (list int)
    ~msg: "run produces the expected side-effect"
    ~expected: l
    ~actual: (List.rev !buf)

let tests = [
  test_list_iter;
]

let l = List.init 2_000 Fun.id

let () =
  Bench_utils.run
    ~base: (fun () -> Bench_utils.run_one ~count: 100_000 (fun () -> List.map (fun x -> x + 1) l))
    ~measured: (fun () -> Bench_utils.run_one ~count: 100_000 (fun () -> monadise_1_1 List.map (fun x -> x + 1) l))
