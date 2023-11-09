open Regex
open Expressions

exception Not_nullable of string
exception Invalid_argument of string
exception Lexing_error of string

let rec flatten v = match v with
  | Empty -> ""
  | Chr c -> String.make 1 c
  | Left v -> flatten v
  | Right v -> flatten v
  | Sequ (v1, v2) -> flatten v1 ^ flatten v2
  | Stars vs -> String.concat "" (List.map flatten vs)
  | Rec (_, v) -> flatten v
  | Opt v -> flatten v

 

let rec env v = match v with
  | Empty -> []
  | Chr _ -> []
  | Left v -> env v
  | Right v -> env v
  | Sequ (v1, v2) -> env v1 @ env v2
  | Stars vs -> List.concat (List.map env vs)
  | Rec (x, v) -> (x, flatten v) :: env v
  | Opt v -> env v

 
let rec mkeps r = match r with
| One -> Empty
| Alt (r1, r2) -> if nullable r1 then Left (mkeps r1) else Right (mkeps r2)
| Seq (r1, r2) -> Sequ (mkeps r1, mkeps r2)
| Star _ -> Stars []
| Recd (x, r) -> Rec (x, mkeps r)
| Plus r -> Stars [mkeps r]
| Optional r -> Opt (mkeps r)
| Ntimes (r, n) -> if n = 0 then Stars [] else Stars (List.init n (fun _ -> mkeps r))
| _ -> failwith "mkeps: not nullable"

let rec inj r c v = match (r, v) with
| (Star r, Sequ (v1, Stars vs)) -> Stars (inj r c v1 :: vs)
| (Seq (r1, _), Sequ (v1, v2)) -> Sequ (inj r1 c v1, v2)
| (Seq (r1, _), Left (Sequ (v1, v2))) -> Sequ (inj r1 c v1, v2)
| (Seq (r1, r2), Right v2) -> Sequ (mkeps r1, inj r2 c v2)
| (Alt (r1, _), Left v1) -> Left (inj r1 c v1)
| (Alt (_, r2), Right v2) -> Right (inj r2 c v2)
| (Char d, Empty) when d = c -> Chr c
| (Recd (x, r1), _) -> Rec (x, inj r1 c v)
| (Range cs, Empty) when List.mem c cs -> Chr c
| (Plus r, Sequ (v1, Stars vs)) -> Stars (inj r c v1 :: vs)
| (Optional r, _) -> Opt (inj r c v)
| (Ntimes (r,_), Sequ (v1, Stars vs)) -> Stars (inj r c v1 :: vs)
| (Ntimes (r, n), _) when n > 0 -> Stars (inj r c v :: List.init (n - 1) (fun _ -> mkeps r))
| _ -> failwith "inj: invalid arguments"


type value_fn = value -> value

let f_id (v: value): value = v
let f_right (f: value_fn) : value_fn = fun v -> Right (f v)
let f_left (f: value_fn) : value_fn = fun v -> Left (f v)
let f_alt (f1: value_fn) (f2: value_fn) : value_fn = fun v ->
  match v with
  | Right v -> Right (f2 v)
  | Left v -> Left (f1 v)
  | _ -> failwith "f_alt: invalid value"
let f_seq (f1: value_fn) (f2: value_fn) : value_fn = fun v ->
  match v with
  | Sequ (v1, v2) -> Sequ (f1 v1, f2 v2)
  | _ -> failwith "f_seq: invalid value"
let f_seq_empty1 (f1: value_fn) (f2: value_fn) : value_fn = fun v ->
  Sequ (f1 Empty, f2 v)
let f_seq_empty2 (f1: value_fn) (f2: value_fn) : value_fn = fun v ->
  Sequ (f1 v, f2 Empty)
let f_recd (f: value_fn) : value_fn = fun v ->
  match v with
  | Rec (x, v) -> Rec (x, f v)
  | _ -> failwith "f_recd: invalid value"
let f_error (_: value): value = raise (Failure "error")

let rec simp (r: rexp) : rexp * value_fn =
  match r with
  | Alt (r1, r2) ->
    let (r1s, f1s) = simp r1 in
    let (r2s, f2s) = simp r2 in
    (match (r1s, r2s) with
     | (Zero, _) -> (r2s, f_right f2s)
     | (_, Zero) -> (r1s, f_left f1s)
     | _ -> if r1s = r2s then (r1s, f_left f1s) else (Alt (r1s, r2s), f_alt f1s f2s))
  | Seq (r1, r2) ->
    let (r1s, f1s) = simp r1 in
    let (r2s, f2s) = simp r2 in
    (match (r1s, r2s) with
     | (Zero, _) | (_, Zero) -> (Zero, f_error)
     | (One, _) -> (r2s, f_seq_empty1 f1s f2s)
     | (_, One) -> (r1s, f_seq_empty2 f1s f2s)
     | _ -> (Seq (r1s, r2s), f_seq f1s f2s))
  | _ -> (r, f_id)


let rec lex_simp r s =
  match s with
  | [] ->
      if nullable r then mkeps r
      else raise (Lexing_error "Not nullable") 
  | c :: cs ->
      let r_simp, f_simp = simp (der c r) in
      inj r c (f_simp (lex_simp r_simp cs))

let lexing_simp r s = env (lex_simp r s)

let while_regs =
  Star (
    ("Keyword" $ keyword)
    |: ("Operator" $ op)
    |: ("Letter" $ letters)
    |: ("Semicolon" $ semi)
    |: ("Parenthesis" $ (lparen |: rparen))
    |: ("Brace" $ (lbrace |: rbrace))
    |: ("Symbol" $ symbols)
    |: ("String" $ string_rexp)
    |: ("Whitespace" $ whitespace)
    |: ("Identifier" $ identifier)
    |: ("Number" $ number)
    |: ("Comment" $ comment)
  )

type token =
  | T_KEYWORD of string
  | T_OP of string
  | T_LETTER of string
  | T_STRING of string
  | T_PAREN of string
  | T_BRACE of string
  | T_SEMI
  | T_ID of string
  | T_NUM of int

let token_func = function
  | "Keyword", s -> T_KEYWORD s
  | "Operator", s -> T_OP s
  | "Letter", s -> T_LETTER s
  | "String", s -> T_STRING s
  | "Parenthesis", s -> T_PAREN s
  | "Brace", s -> T_BRACE s
  | "Semicolon", _ -> T_SEMI
  | "Identifier", s -> T_ID s
  | "Number", s -> T_NUM (int_of_string s)
  | _, _ -> raise (Invalid_argument "Invalid tag for token")

let string_of_token = function
| T_KEYWORD s -> Printf.sprintf "T_KEYWORD %s" s
| T_OP s -> Printf.sprintf "T_OP %s" s
| T_LETTER s -> Printf.sprintf "T_LETTER %s" s
| T_STRING s -> Printf.sprintf "T_STRING %s" s
| T_PAREN s -> Printf.sprintf "T_PAREN %s" s
| T_BRACE s -> Printf.sprintf "T_BRACE %s" s
| T_SEMI -> "T_SEMI"
| T_ID s -> Printf.sprintf "T_ID %s" s
| T_NUM n -> Printf.sprintf "T_NUM %d" n


let tokenise s =
  let vals = lexing_simp while_regs s in
  List.filter_map
    (fun pair -> try Some (token_func pair) with Invalid_argument _ -> None)
    vals
