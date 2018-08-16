{

type options =
[ `Pad of char
| `NoPad
| `None
| `Upper
| `Tz of int ]

type t =
  | String of string
  | Directive of options * char
  | Space
  | EOL
}

let directive = ['a'-'z' 'A'-'Z']

      
rule tokens = parse
  | "%%" {String "%"}
  | '%' {
      let opt = options lexbuf in
      let dir = directive lexbuf in
      Directive (opt,dir)
    }
  | [ '\n' '\t' ' ' ] * eof { EOL }
  | [ '\n' '\t' ' ' ] + { Space }
  | [^ '%' '\n' '\t' ' ' ] + { String (Lexing.lexeme lexbuf)}

and directive = parse
  | ['a'-'z' 'A'-'Z'] {Lexing.lexeme_char lexbuf 0}

and options = parse
  | '_'   { `Pad ' ' }
  | '0'   { `Pad '0' }
  | '-'   { `NoPad }
  | '^'   { `Upper }
  | ":::" { `Tz 3 }
  | "::"  { `Tz 2 }
  | ':'   { `Tz 1 }
  | ""    { `None }
