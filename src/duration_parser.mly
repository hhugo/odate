
%token <char> DIRECTIVE CHAR
%token <int> INT
%token DASH OPEN CLOSE SEMICOLON PERCENT
%token EOF EQ LT GT

%start main             /* the entry point */

%type < Duration_private.O.t -> Duration_private.O.t -> string > main


%%

main: r=exprs(undefined_value) EOF {r}

exprs(VD): l=list(expr(VD))
  { (fun a b -> String.concat "" (List.map (fun f -> f a b) l)) };

expr(VD):
  | i=VD { Duration_private.show_value i }
  | OPEN;dir=DIRECTIVE;CLOSE
    { (fun d _v ->
      let _,v = Duration_private.apply_directive dir d in
      Duration_private.O.to_string v) }
  | OPEN;PERCENT;check=simple_condition;SEMICOLON ; x=exprs(VD); CLOSE
    { Duration_private.check_condition_simple check x }
  | OPEN;DASH;cmp=condition;i=INT;SEMICOLON;x=exprs(VD); y=option(else_expr(VD));CLOSE
    { Duration_private.check_condition cmp i x y }
  | OPEN; dir=DIRECTIVE; SEMICOLON; x=exprs(defined_value); CLOSE
    { Duration_private.directive_block dir x }
  | l=nonempty_list(charlike) {
    let size = List.length l in
    let s = Bytes.create size in
    List.iteri (fun i x -> Bytes.set s i x) l;
    Duration_private.static_printer (Bytes.to_string s)
  }

defined_value:
    | l=nonempty_list(DASH) { List.length l }

undefined_value:
    | x=nonempty_list(DASH) {failwith (Printf.sprintf "%s not authorized ouside block [%%d: ] (at c%d)" (String.make (List.length x) '#' ) ($startpos).Lexing.pos_cnum)}

charlike:
  | x=CHAR {x}
  | x=INT  {char_of_int (x + 48) }
  | LT     { '<' }
  | GT     { '>' }
  | EQ     { '=' }

else_expr(VD):
  | SEMICOLON;x=exprs(VD) { x }

condition:
  | LT GT { `NEQ }
  | EQ { `EQ }
  | LT { `LT }
  | GT { `GT }

simple_condition:
  | i=INT {`EQ i}
  | LT { `LT }
  | GT   { `GT }
;


%%
