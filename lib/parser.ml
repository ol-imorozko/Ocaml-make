(** Copyright 2021-2022, ol-imorozko and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

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

let some_pred preds el = List.exists (fun fn -> fn el) preds
let empty_lines = skip_while (some_pred [ is_whitespace; is_tab; is_delim ])

(* returns list of exprs.*)

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

(* Skip empty lines *)
let parser = many1 (empty_lines *> expr) <* empty_lines
let parse_ok = test_ok (Format.pp_print_list pp_expr) parser
let parse_fail = test_fail (Format.pp_print_list pp_expr) parser

let%test _ = parse_ok "t" [ Rule tmp ]
let%test _ = parse_ok "     t" [ Rule tmp ]
let%test _ = parse_ok "     t   t" [ Rule tmp; Rule tmp ]
let%test _ = parse_ok "tt \t \t \n t" [ Rule tmp; Rule tmp; Rule tmp ]
let%test _ = parse_ok "     t             " [ Rule tmp ]
let%test _ = parse_ok "     t   \t  \n  t   " [ Rule tmp; Rule tmp ]
let%test _ = parse_ok "\n\t\t  \tt" [ Rule tmp ]
let%test _ = parse_ok " \n\n  \n  t" [ Rule tmp ]
let%test _ = parse_ok "     t   t" [ Rule tmp; Rule tmp ]
let%test _ = parse_ok "     tt" [ Rule tmp; Rule tmp ]
let%test _ = parse_ok "     t   t" [ Rule tmp; Rule tmp ]
let%test _ = parse_fail "     x   f   "
let%test _ = parse_fail " \t \t \n "
let%test _ = parse_fail ""

let empty_lines =
  let some_pred preds el = List.exists (fun fn -> fn el) preds in
  take_while1 (some_pred [ is_whitespace; is_tab; is_delim ]) *> return ()
;;

(* Skip comment/empty lines *)
let comment = char '#' *> skip_while (Fun.negate is_delim) <* many (skip is_delim)
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

let all_pred preds el = List.for_all (fun fn -> fn el) preds

let target =
  take_while1
    (all_pred
       [ Fun.negate is_whitespace
       ; Fun.negate is_tab
       ; Fun.negate is_delim
       ; Fun.negate is_hash
       ; Fun.negate is_colon
       ])
;;

let empty_symbols =
  let empty_symbols1 = take_while1 (some_pred [ is_whitespace; is_tab ]) *> return () in
  option () empty_symbols1
;;

let targets = both (target <* empty_symbols) (many (target <* empty_symbols)) <* char ':'

type targets = string * string list [@@deriving show { with_path = false }]

(* Parse targets *)
let parser = targets
let parse_ok = test_ok pp_targets parser
let parse_fail = test_fail pp_targets parser

let%test _ = parse_ok "abc:" ("abc", [])
let%test _ = parse_ok "abc  :" ("abc", [])
let%test _ = parse_ok "abc \t  :" ("abc", [])
let%test _ = parse_ok "a \t b c,;dex :" ("a", [ "b"; "c,;dex" ])
let%test _ = parse_ok "abc \t f  f :" ("abc", [ "f"; "f" ])
let%test _ = parse_fail ":"
let%test _ = parse_fail ":::"
let%test _ = parse_fail ""
let%test _ = parse_fail ":  \t fdsf \n"
