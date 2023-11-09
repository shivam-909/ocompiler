open OUnit2

let () =
  let suite =
    "All Tests"
    >::: [
           "Lexer Expressions Tests" >::: [  ];
           (* Add other test suites here as you create them *)
         ]
  in
  run_test_tt_main suite
