open Regex

let string_to_list s =
  let rec aux i acc = if i < 0 then acc else aux (i - 1) (s.[i] :: acc) in
  aux (String.length s - 1) []

let rec string_to_regex s =
  match string_to_list s with
  | [] -> One
  | c :: cs ->
      Seq
        ( Char c,
          string_to_regex (String.concat "" (List.map (String.make 1) cs)) )

let letters = Range (string_to_list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
let digits = Range (string_to_list "0123456789")
let symbols = Range (string_to_list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz._><=;,:\\")

let whitespace = 
  Plus (Char ' ')
  |: (string_to_regex "\n")
  |: (string_to_regex "\t")
  |: ((string_to_regex "\\") -: (Char 'n'))
  |: ((string_to_regex "\\") -: (Char 't'))

let keyword = 
  string_to_regex "while"
  |: string_to_regex "if"
  |: string_to_regex "then"
  |: string_to_regex "else"
  |: string_to_regex "do"
  |: string_to_regex "for"
  |: string_to_regex "to"
  |: string_to_regex "true"
  |: string_to_regex "false"
  |: string_to_regex "read"
  |: string_to_regex "write"
  |: string_to_regex "skip"

let op = 
  string_to_regex "+"
  |: string_to_regex "-"
  |: string_to_regex "*"
  |: string_to_regex "%"
  |: string_to_regex "/"
  |: string_to_regex "=="
  |: string_to_regex "!="
  |: string_to_regex ">"
  |: string_to_regex "<"
  |: string_to_regex "<="
  |: string_to_regex ">="
  |: string_to_regex ":="
  |: string_to_regex "&&"
  |: string_to_regex "||"


let semi = Char ';'
let lparen = Char '('
let rparen = Char ')'
let lbrace = Char '{'
let rbrace = Char '}'

let string_content = Star(symbols |: whitespace |: digits)
let string_rexp = (string_to_regex "\"") -: string_content -: (string_to_regex "\"")
let identifier = letters -: Plus (Char '_' |: letters |: digits)
let non_zero_digit = Range (string_to_list "123456789")
let number = Seq (non_zero_digit, Star digits) |: Char '0'
let comment = string_to_regex "//" -: Star (symbols |: digits |: Char ' ') -: string_to_regex "\n"
