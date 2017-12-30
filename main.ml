open Angstrom

module StringMap = Map.Make(struct
    type t = string
    let compare = compare
  end)

let tuples_into_stringmap tuples =
  List.fold_left
    (fun m (k, v) -> StringMap.add k v m)
    StringMap.empty
    tuples

module Parser = struct

  let isSpace = function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false

  let rec skipSpaces = take_while isSpace

  let lexeme p = p <* skipSpaces

  let field = lexeme @@
    let key = take_while1 (function | '=' -> false | _ -> true) <* char '=' in
    let value = many_till any_char (end_of_input <|> end_of_line) in
    lift2 (fun k v -> (k, Core.Std.String.of_char_list v))
      key
      value

  let section_name = lexeme @@
    char '[' *> take_while1 (function | ']' -> false | _ -> true) <* char ']'

  let peek_char_lsb =
    peek_char >>= function
    | Some '[' -> return ()
    | _ -> fail "was not lsb"

  let section = lexeme @@
    let name = section_name in
    let fields = many_till field (peek_char_lsb <|> end_of_input) in
    lift2 (fun n fs -> (n, tuples_into_stringmap fs))
      name
      fields

  let document =
    skipSpaces *> many_till section end_of_input >>| tuples_into_stringmap

  let parse_string =
    parse_only document
end

let () =
  match parse_only Parser.document (`String "[apple]\nkiwi=grape\n[banana]") with
  | Ok a ->
    let _ = StringMap.mapi (fun k x ->
        let _ = print_string k in
        StringMap.mapi (fun l y ->
            print_endline (l ^ " " ^ y)
          ) x
      ) a in
    print_endline ""
  | Error e -> print_endline @@ "error: " ^ e
