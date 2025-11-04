include Monadise.Make(struct
  type 'a t = 'a option
  let return x = Some x
  let bind' ~on_error x f =
    match x with
    | None -> on_error (); None
    | Some x -> f x
end)

open Alcotest

let test_list_iter =
  test_case "monadise_1_1 List.iter" `Quick @@ fun () ->
  let l = [1; 2; 3; 4; 5] in
  let buf = ref [] in
  check'
    (option unit)
    ~msg: "run succeeds"
    ~expected: (Some ())
    ~actual: (monadise_1_1 List.iter (fun x -> buf := x :: !buf; Some ()) l);
  check'
    (list int)
    ~msg: "run produces the expected side-effect"
    ~expected: l
    ~actual: (List.rev !buf);
  buf := [];
  check'
    (option unit)
    ~msg: "run with error fails as expected"
    ~expected: None
    ~actual: (monadise_1_1 List.iter (fun x -> buf := x :: !buf; if x = 3 then None else Some ()) l);
  check'
    (list int)
    ~msg: "run with error produces the expected side-effect"
    ~expected: [1; 2; 3]
    ~actual: (List.rev !buf)

let test_array_map =
  test_case "monadise_2_1 Array.mapi" `Quick @@ fun () ->
  let a = [|3; 5; 1; 2; 3|] in
  check'
    (option @@ array int)
    ~msg: "run produces the expected output"
    ~expected: (Some [|3; 6; 3; 5; 7|])
    ~actual: (monadise_2_1 Array.mapi (fun i x -> Some (i + x)) a);
  check'
    (option @@ array int)
    ~msg: "run with error produces the expected output"
    ~expected: None
    ~actual: (monadise_2_1 Array.mapi (fun i x -> if i = 3 then None else Some (i + x)) a)

let test_seq_fold_left =
  test_case "monadise_3_2 Seq.fold_lefti" `Quick @@ fun () ->
  let s = List.to_seq [3; 7; 0; 1; 4] in
  check'
    (option int)
    ~msg: "run produces the expected output"
    ~expected: (Some 5)
    ~actual: (monadise_3_2 Seq.fold_lefti (fun acc i x -> Some (acc + x - i)) 0 s);
  check'
    (option int)
    ~msg: "run produces the expected output"
    ~expected: None
    ~actual: (monadise_3_2 Seq.fold_lefti (fun acc i x -> if i = 3 then None else Some (acc + x - i)) 0 s)

let test_list_sort =
  test_case "monadise_2_1 List.sort" `Quick @@ fun () ->
  let l = [3; 5; 1; 4; 2] in
  check'
    (option @@ list int)
    ~msg: "run produces the expected output"
    ~expected: (Some (List.sort Int.compare l))
    ~actual: (monadise_2_1 List.sort (fun x y -> Some (Int.compare x y)) l)

let tests = [
  test_list_iter;
  test_array_map;
  test_seq_fold_left;
  test_list_sort;
]
