{
open Duration_parser
}

let directive = [ 's' 'm' 'h' 'D' 'W' 'M' 'Y' 'x']
let special = [ '%' '#' '[' ']' ':' ]
let other = [ ^ ']' ':' ]

rule tokens = parse
  | '<'  {LT}
  | '>'  {GT}
  | '='  {EQ}
  | '%' {PERCENT}
  | '%' directive { DIRECTIVE (Lexing.lexeme_char lexbuf 1) }
  | '#' { DASH }
  | "\\" special { CHAR (Lexing.lexeme_char lexbuf 1) }
  | '['  { OPEN }
  | ']'  { CLOSE }
  | ':'  { SEMICOLON }
  | [ '\n' '\t' ' ' ] * eof { EOF }
  | ['0' - '9'] + {INT (int_of_string (Lexing.lexeme lexbuf))}
  | other { CHAR (Lexing.lexeme_char lexbuf 0) }
