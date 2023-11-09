type rexp =
  | Zero
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
  | Empty
  | Chr of char
  | Sequ of value * value
  | Left of value
  | Right of value
  | Stars of value list
  | Rec of string * value
  | Opt of value

let ( -: ) r s = Seq (r, s)
let ( %: ) r = Star r
let ( |: ) r s = Alt (r, s)
let ( $ ) c r = Recd (c, r)

let rec rexpstring r = match r with
  | Zero -> "Zero"
  | One -> "One"
  | Char c -> "Char " ^ Char.escaped c
  | Alt (r1, r2) -> "Alt (" ^ rexpstring r1 ^ ", " ^ rexpstring r2 ^ ")"
  | Seq (r1, r2) -> "Seq (" ^ rexpstring r1 ^ ", " ^ rexpstring r2 ^ ")"
  | Star r -> "Star (" ^ rexpstring r ^ ")"
  | Recd (s, r) -> "Recd (" ^ s ^ ", " ^ rexpstring r ^ ")"
  | Range cs -> "Range [" ^ String.concat ", " (List.map Char.escaped cs) ^ "]"
  | Plus r -> "Plus (" ^ rexpstring r ^ ")"
  | Optional r -> "Optional (" ^ rexpstring r ^ ")"
  | Ntimes (r, n) -> "Ntimes (" ^ rexpstring r ^ ", " ^ string_of_int n ^ ")"
  | Cfun _ -> "Cfun"

let rec nullable r = match r with
  | Zero -> false
  | One -> true
  | Char _ -> false
  | Alt (r1, r2) -> nullable r1 || nullable r2
  | Seq (r1, r2) -> nullable r1 && nullable r2
  | Star _ -> true
  | Recd (_, r1) -> nullable r1
  | Range _ -> false
  | Plus r -> nullable r
  | Optional _ -> true
  | Ntimes (r, n) -> if n = 0 then true else nullable r
  | Cfun _ -> false

let rec der c r = match r with
  | Zero -> Zero
  | One -> Zero
  | Char d -> if c = d then One else Zero
  | Alt (r1, r2) -> Alt (der c r1, der c r2)
  | Seq (r1, r2) -> 
      if nullable r1 then Alt (Seq (der c r1, r2), der c r2)
      else Seq (der c r1, r2)
  | Star r -> Seq (der c r, Star r)
  | Recd (_, r1) -> der c r1
  | Range cs -> if List.mem c cs then One else Zero
  | Plus r -> Seq (der c r, Star r)
  | Optional r -> der c r
  | Ntimes (_, 0) -> Zero
  | Ntimes (r, n) -> Seq (der c r, Ntimes (r, n - 1))
  | Cfun f -> if f c then One else Zero
