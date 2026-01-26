open Simpl
open Eval
open Pp
open Syntax

let parse_buf (lexbuf : Lexing.lexbuf) : term =
  lexbuf |> Parser.prog Lexer.read |> remove_names empty_context

let () =
  let input_channel = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel input_channel in
  try
    let nameless_ast = parse_buf lexbuf in
    let result = steps nameless_ast in
    close_in input_channel;
    print_term empty_context result
    (* Sexplib.Sexp.pp_hum Format.std_formatter (sexp_of_term nameless_ast) *)
  with
  | Lexer.Error msg -> Printf.eprintf "Lexical error: %s\n" msg
  | Parser.Error ->
      Printf.eprintf "Syntax error at offset %d\n" (Lexing.lexeme_start lexbuf)
  | e ->
      close_in_noerr input_channel;
      raise e
