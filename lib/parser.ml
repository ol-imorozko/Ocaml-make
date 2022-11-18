(** Copyright 2021-2022, ol-imorozko and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
(* open Printf *)

let is_whitespace = function
  | ' ' -> true
  | _ -> false
;;

let is_tab = function
  | '\t' -> true
  | _ -> false
;;

let is_delim = function
  | '\n' | '\r' -> true
  | _ -> false
;;

let is_hash = function
  | '#' -> true
  | _ -> false
;;

let is_colon = function
  | ':' -> true
  | _ -> false
;;

let is_backslash = function
  | '\\' -> true
  | _ -> false
;;

let some_pred preds el = List.exists (fun fn -> fn el) preds
let all_pred preds el = List.for_all (fun fn -> fn el) preds

let test_ok, test_fail =
  let ok ppf parser input expected =
    match parse_string ~consume:All parser input with
    | Ok res when expected = res -> true
    | Ok res ->
      ppf Format.std_formatter res;
      false
    | Error e ->
      print_string e;
      false
  in
  let fail ppf parser input =
    match parse_string ~consume:All parser input with
    | Ok res ->
      ppf Format.std_formatter res;
      false
    | _ -> true
  in
  ok, fail
;;

let tmp = { targets = "abc", []; prerequisites = []; recipe = [] }
let expr = char 't' >>| fun _ -> Rule tmp

(* skips fully empty lines *)
let empty_lines =
  let some_pred preds el = List.exists (fun fn -> fn el) preds in
  take_while1 (some_pred [ is_whitespace; is_tab; is_delim ]) *> return ()
;;

(* Skips full line starting with # *)
let comment = char '#' *> skip_while (Fun.negate is_delim) <* many (skip is_delim)

(* skips fully empty lines or comment lines *)
let skip_meaningless_lines = many (empty_lines <|> comment)
let parser = many1 (skip_meaningless_lines *> expr) <* skip_meaningless_lines
let parse_ok = test_ok (Format.pp_print_list pp_expr) parser
let parse_fail = test_fail (Format.pp_print_list pp_expr) parser

let%test _ = parse_ok "#\nt" [ Rule tmp ]
let%test _ = parse_ok "t#" [ Rule tmp ]
let%test _ = parse_ok "t#\n" [ Rule tmp ]
let%test _ = parse_ok "#\n #\n t #\n #\n t" [ Rule tmp; Rule tmp ]
let%test _ = parse_ok "#\n   #\n    #fdsf\n t" [ Rule tmp ]
let%test _ = parse_ok " #fdsfs\n    t    #fdfsf" [ Rule tmp ]
let%test _ = parse_ok " #fdsfs\n    t    #fdfsf  \n  " [ Rule tmp ]
let%test _ = parse_ok "  #39139\r   t   t  #" [ Rule tmp; Rule tmp ]
let%test _ = parse_fail "#"
let%test _ = parse_fail "    #fodsfjo \r#x  \n# f"
let%test _ = parse_fail "#fodsfjo\n \t \t \n "

(* let expr = func <|> rule *)

let target =
  take_while1
    (all_pred
       [ Fun.negate is_whitespace
       ; Fun.negate is_tab
       ; Fun.negate is_delim
       ; Fun.negate is_hash
       ; Fun.negate is_colon
       ; Fun.negate is_backslash
       ])
;;

(* skips empty symbols (but not newlines) *)
let empty_symbols =
  let empty_symbols1 = take_while1 (some_pred [ is_whitespace; is_tab ]) *> return () in
  option () empty_symbols1
;;

(* parses targets `<target> [<target[s]>...]:` *)
let targets = both (target <* empty_symbols) (many (target <* empty_symbols)) <* char ':'

type targets = string * string list [@@deriving show { with_path = false }]

let parser = targets
let parse_ok = test_ok pp_targets parser
let parse_fail = test_fail pp_targets parser

let%test _ = parse_ok "abc:" ("abc", [])
let%test _ = parse_ok "abc  :" ("abc", [])
let%test _ = parse_ok "abc \t  :" ("abc", [])
let%test _ = parse_ok "a\tb:" ("a", [ "b" ])
let%test _ = parse_ok "a \t b c,;dex :" ("a", [ "b"; "c,;dex" ])
let%test _ = parse_ok "abc \t f  f :" ("abc", [ "f"; "f" ])
let%test _ = parse_fail "abc\n:"
let%test _ = parse_fail "abc#:"
let%test _ = parse_fail ":#c:"
let%test _ = parse_fail ":"
let%test _ = parse_fail ":::"
let%test _ = parse_fail ""
let%test _ = parse_fail ":  \t fdsf \n"

(* Parse prerequisites (could be multilined) *)
(* ban '\' in filenames for now *)
let prerequisites =
  let trim_empty p = empty_symbols *> p <* empty_symbols in
  let prereq_on_one_line = trim_empty (sep_by empty_symbols target) in
  let eol = many1 (end_of_line <|> comment) in
  fix (fun p ->
    lift2
      List.append
      prereq_on_one_line
      (eol *> return [] <|> (char '\\' <* empty_symbols *> char '\n') *> p <|> return []))
;;

let parser = prerequisites
let parse_ok = test_ok (Format.pp_print_list (fun _ -> print_string)) parser
let parse_fail = test_fail (Format.pp_print_list (fun _ -> print_string)) parser

let%test _ = parse_ok "abc\n" [ "abc" ]
let%test _ = parse_ok "abc" [ "abc" ]
let%test _ = parse_ok "abc  " [ "abc" ]
let%test _ = parse_ok "abc \t  " [ "abc" ]
let%test _ = parse_ok "a \t b c,;dex" [ "a"; "b"; "c,;dex" ]
let%test _ = parse_ok "abc \t f  f " [ "abc"; "f"; "f" ]
let%test _ = parse_ok "abc #\t f  f " [ "abc" ]
let%test _ = parse_ok "abc #\n" [ "abc" ]
let%test _ = parse_ok "#:f" []
let%test _ = parse_ok "" []
let%test _ = parse_ok "  \n" []
let%test _ = parse_fail "\na"
let%test _ = parse_fail "\n\n\n a"
let%test _ = parse_fail ":"
let%test _ = parse_fail ":fsdf"
let%test _ = parse_fail "fdsf:::"
(* multiline tests *)
let%test _ = parse_ok "a\\\nb" [ "a"; "b" ]
let%test _ = parse_ok "  \\  \t \n  a" [ "a" ]
let%test _ = parse_ok "  \\\n  a\n" [ "a" ]
let%test _ = parse_ok "a\\\n  \t  \t \n" [ "a" ]
let%test _ = parse_ok "a\\\n  \t  \t b  " [ "a"; "b" ]
let%test _ = parse_ok "a  \\\n  b" [ "a"; "b" ]
let%test _ = parse_ok "a a\\b" [ "a"; "a\\b" ]
let%test _ = parse_ok "a\\\n" [ "a" ]
let%test _ = parse_ok "a\\\n\na" [ "a" ]
let%test _ = parse_ok "a\\\n\n  abc:" [ "a" ]
let%test _ = parse_ok "a \\\n b c \\\n d e" [ "a"; "b"; "c"; "d"; "e" ]
let%test _ = parse_fail "a\\\n  b:"

(* combine these two to make <target> [<target[s]>...]: [<prerequisite[s]>...] parsing *)
type rule_wo_recepie =
  { targets : string * string list
  ; prerequisites : string list
  }
[@@deriving show { with_path = false }]

(* let parse_ok = test_ok pp_rule_wo_recepie parser *)
(* let parse_fail = test_fail pp_rule_wo_recepie parser *)

(* let%test _ = parse_ok "a:b\n" { targets = "a", []; prerequisites = [ "b" ] } *)
