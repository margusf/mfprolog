open Common

let load_rules filename =
  try
    let lexbuf = Lexing.from_channel (open_in filename) in
      Grammar.rulelist Lexer.token lexbuf
  with
    | Sys_error err ->
    debug ("IO error: " ^ err);
    raise (Prolog_error ("Cannot read from file: " ^ filename))

let merge_loaded new_rules =
  (* TODO: incremental loading *)
  rules := new_rules

let consult_file filename =
  let loaded = load_rules (filename ^ ".pl") in
    merge_loaded loaded