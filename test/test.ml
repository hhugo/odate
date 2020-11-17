module Date = ODate.Unix
module Duration = ODuration

let format = "%a %b %d %T %:::z %Y"

let parseR =
  match Date.From.generate_parser format with
  | Some p -> p
  | None -> failwith "could not generate parser"

let printer =
  match Date.To.generate_printer format with
  | Some p -> p
  | None -> failwith "could not generate printer"

let%expect_test "test1" =
  let a = Date.now () in
  let d1 = Duration.From.ms 9990 in
  let b = Date.now () in
  let d2 = Date.between a b in
  let d3 = Date.between b a in
  let s1 = Duration.To.string Duration.To.default_printer d2 in
  let s2 = Duration.To.string Duration.To.default_printer d1 in
  let s3 = Duration.To.string Duration.To.default_printer d3 in
  print_endline s1;
  [%expect {| just now |}];
  print_endline s2;
  [%expect {| in 9 seconds |}];
  print_endline s3;
  [%expect {| just now |}]

let%expect_test "test2" =
  let d_string = "Wed May 29 20:20:23 +00 2013" in
  let d = Date.From.string parseR d_string in
  let s = Date.To.string printer d in
  print_endline s;
  [%expect {| Wed May 29 20:20:23 +00 2013 |}]

let%expect_test "test3" =
  let s = Date.From.string parseR "Wed May 29 20:20:23 +00 2013" in
  let s''' = Date.To.string ~tz:ODate.Local printer s in
  print_endline s''';
  (* let s = Date.advance_by_days s (-2) in *)
  for i = 0 to 2 do
    let s' = Date.advance_by_months s i in
    (* let s'' = s' in *)
    let s'' = Date.end_of_the_month ~tz:ODate.Local s' in
    let s''' = Date.To.string ~tz:ODate.Local printer s'' in
    print_endline s'''
  done;
  [%expect {|
    Wed May 29 21:20:23 +01 2013
    Fri May 31 21:20:23 +01 2013
    Sun Jun 30 21:20:23 +01 2013
    Wed Jul 31 21:20:23 +01 2013 |}]
