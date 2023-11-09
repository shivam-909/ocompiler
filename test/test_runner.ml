open OUnit2

let () =
  let suite =
    "All Tests"
    >::: [
           "Lexer Expressions Tests" >::: [  ];
         ]
  in
  run_test_tt_main suite
