open Lexer.Lexing
open Io.Files
let string_to_char_list s = List.of_seq (String.to_seq s)

let main () =
  let filenames = [
    "./data/collatz.while";
    "./data/collatz2.while";
    "./data/fib.while";
    "./data/loops.while";
    "./data/factors.while";
    "./data/primes.while";
  ] in
  let read_files_with_names file_list =
    List.filter_map (fun filename ->
      match read_file_safe filename with
      | Some content -> Some (filename, content)
      | None -> None
    ) file_list
  in
  let file_contents_with_names = read_files_with_names filenames in
  List.iter (fun (name, contents) ->
    print_endline (name ^ ":\n");  
    let tokens = tokenise (string_to_char_list contents) in
    List.iter (fun t -> print_endline (string_of_token t)) tokens;
    print_endline "\n\n\n"  
  ) file_contents_with_names

let () = main ()
