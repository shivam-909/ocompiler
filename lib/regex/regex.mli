type rexp =
    Zero
  | One
  | Char of char
  | Alt of rexp * rexp
  | Seq of rexp * rexp
  | Star of rexp
  | Recd of string * rexp
  | Range of char list
  | Plus of rexp
  | Optional of rexp
  | Ntimes of rexp * int
  | Cfun of (char -> bool)
type value =
    Empty
  | Chr of char
  | Sequ of value * value
  | Left of value
  | Right of value
  | Stars of value list
  | Rec of string * value
  | Opt of value
val ( -: ) : rexp -> rexp -> rexp
val ( %: ) : rexp -> rexp
val ( |: ) : rexp -> rexp -> rexp
val ( $ ) : string -> rexp -> rexp
val rexpstring : rexp -> string
val nullable : rexp -> bool
val der : char -> rexp -> rexp
