
let prompt = "?- "

let run_repl () =
  let lexbuf = Lexing.from_channel stdin in
  let rec do_term () =
    print_string prompt;
    flush stdout;
    let con = Grammar.conjunct Lexer.token lexbuf in
      Inference.solve con;
      do_term () in
    do_term()
