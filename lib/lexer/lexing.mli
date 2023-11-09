exception Not_nullable of string
exception Invalid_argument of string
exception Lexing_error of string
val flatten : Regex.value -> string
val env : Regex.value -> (string * string) list
val mkeps : Regex.rexp -> Regex.value
val inj : Regex.rexp -> char -> Regex.value -> Regex.value
type value_fn = Regex.value -> Regex.value
val f_id : Regex.value -> Regex.value
val f_right : value_fn -> value_fn
val f_left : value_fn -> value_fn
val f_alt : value_fn -> value_fn -> value_fn
val f_seq : value_fn -> value_fn -> value_fn
val f_seq_empty1 : value_fn -> value_fn -> value_fn
val f_seq_empty2 : value_fn -> value_fn -> value_fn
val f_recd : value_fn -> value_fn
val f_error : Regex.value -> Regex.value
val simp : Regex.rexp -> Regex.rexp * value_fn
val lex_simp : Regex.rexp -> char list -> Regex.value
val lexing_simp : Regex.rexp -> char list -> (string * string) list
val while_regs : Regex.rexp
type token =
    T_KEYWORD of string
  | T_OP of string
  | T_LETTER of string
  | T_STRING of string
  | T_PAREN of string
  | T_BRACE of string
  | T_SEMI
  | T_ID of string
  | T_NUM of int
val token_func : string * string -> token
val string_of_token : token -> string
val tokenise : char list -> token list
