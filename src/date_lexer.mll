{
open Lexing
type t =
  | String of string
  | Directive of char option * char
  | Space
  | EOL
}

let padding = ['_' '-' '0']
let directive = ['a'-'z' 'A'-'Z']

rule tokens = parse
  | "%%" {String "%"}
  | '%' padding ? directive {
      let c1 = (Lexing.lexeme_char lexbuf 1) in
      let padding, directive =
        if c1 = '-' || c1 = '_' || c1 = '0'
        then Some c1, (Lexing.lexeme_char lexbuf 2)
        else None, c1 in
      Directive (padding,directive)
    }
  | [ '\n' '\t' ' ' ] * eof { EOL }
  | [ '\n' '\t' ' ' ] + { Space }
  | [^ '%' '\n' '\t' ' ' ] + { String (Lexing.lexeme lexbuf)}
