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

(* is newline or carriage return character *)
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

let empty_lines =
  let some_pred preds el = List.exists (fun fn -> fn el) preds in
  skip_while (some_pred [ is_whitespace; is_tab; is_delim ])
;;

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

(* Test that tests that empty_lines parser work *)
let tmp = { targets = "abc", []; prerequisites = []; recipe = [] }
let expr = char 'k' >>| fun _ -> Rule tmp
let parser = many1 (empty_lines *> expr)
let parse_ok = test_ok (Format.pp_print_list pp_expr) parser
let parse_fail = test_fail (Format.pp_print_list pp_expr) parser

let%test _ = parse_ok "     k" [ Rule tmp ]
let%test _ = parse_ok "     k   k" [ Rule tmp; Rule tmp ]
let%test _ = parse_ok "kk \t \t \n k" [ Rule tmp; Rule tmp; Rule tmp ]
let%test _ = parse_ok "     k             " [ Rule tmp ]
let%test _ = parse_ok "     k   \t  \n  k   " [ Rule tmp; Rule tmp ]
let%test _ = parse_ok "\n\t\t  \tk" [ Rule tmp ]
let%test _ = parse_ok " \n\n  \n  k" [ Rule tmp ]
let%test _ = parse_ok "     k   k" [ Rule tmp; Rule tmp ]
let%test _ = parse_ok "     kk" [ Rule tmp; Rule tmp ]
let%test _ = parse_ok "     k   k" [ Rule tmp; Rule tmp ]
let%test _ = parse_fail "     x   f"
let%test _ = parse_fail " \t \t \n "
let%test _ = parse_fail ""

(* comment_lines parser work *)
let comment_lines = char '#' *> skip_while (Fun.negate is_delim) *> skip is_delim
let parser = many1 (many (empty_lines *> comment_lines) *> expr)

let%test _ = parse_ok " #fdsfs\n    k" [ Rule tmp ]
let%test _ = parse_ok "  #39139\r   k   k" [ Rule tmp; Rule tmp ]
let%test _ = parse_fail "    #fodsfjo x   f"
let%test _ = parse_fail "#fodsfjo\n \t \t \n "
let%test _ = parse_fail ""
