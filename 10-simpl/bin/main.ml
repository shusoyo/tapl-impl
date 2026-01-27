open Simpl
open Eval
open Pp
open Syntax

let parse_buf (lexbuf : Lexing.lexbuf) : n_term =
  lexbuf |> Parser.prog Lexer.read

let rec trace_steps (t : term) : term list =
  if is_val t then
    [ t ]
  else
    t :: trace_steps (step t)

let () =
  let input_channel = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel input_channel in
  try
    let named_ast = parse_buf lexbuf in
    let nameless_ast = remove_names empty_context named_ast in

    (* Named AST (for readable inspection) *)
    Format.printf "Parsed AST (named):\n";
    Sexplib.Sexp.pp_hum Format.std_formatter (sexp_of_n_term named_ast);
    Format.print_newline ();

    (* Type of the term *)
    let ty = typeof empty_context nameless_ast in
    Format.printf "Type: %s\n" (Sexplib.Sexp.to_string_hum (sexp_of_ty ty));

    (* Evaluation trace (pretty) *)
    Format.printf "Evaluation trace:\n";
    let steps = trace_steps nameless_ast in
    steps
    |> List.iteri (fun i t ->
        Format.printf "---- step %d ----\n" (i + 1);
        (* show S-expression of the named form for clarity *)
        let named = resotre_names empty_context t in
        Format.printf "AST (s-expression):@,@[<v 0>%a@]@." Sexplib.Sexp.pp_hum
          (sexp_of_n_term named);
        Format.printf "Pretty syntax:@,";
        print_term empty_context t;
        Format.print_newline ());

    close_in input_channel
  with
  | Lexer.Error msg -> Printf.eprintf "Lexical error: %s\n" msg
  | Parser.Error ->
      Printf.eprintf "Syntax error at offset %d\n" (Lexing.lexeme_start lexbuf)
  | e ->
      close_in_noerr input_channel;
      raise e
