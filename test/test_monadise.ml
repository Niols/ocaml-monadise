let () =
  Alcotest.run "monadise" [
    ("identity", Test_identity.tests);
    ("option", Test_option.tests);
  ]
