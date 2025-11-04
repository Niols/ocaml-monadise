let () =
  Alcotest.run "monadise" [
    ("option", Test_option.tests);
  ]
