

module Make (Duration : Duration.S) (Date : Date.S with type d = Duration.t) = struct
  open Lwt
  let test1 =
    let a = Date.now () in
    let d1 = Duration.From.ms 999 in
    Duration.sleep d1  >>= fun () ->
    let b = Date.now () in
    let d2 = Date.between a b in
    let d3 = Date.between b a in
    let s1 = Duration.To.string Duration.To.default_printer d2 in
    let s2 = Duration.To.string Duration.To.default_printer d1 in
    let s3 = Duration.To.string Duration.To.default_printer d3 in
    let _ = print_endline s1 in
    let _ = print_endline s2 in
    let _ = print_endline s3 in
    let format = match Date.From.generate_parser "%a %b %d %T %z %Y" with
      | Some p -> p
      | None -> failwith "could not generate parser" in
    let _ =
      let d = Date.From.string format "Wed Aug 27 13:08:45 +0000 2008" in
      let d = Date.From.string format "Wed May 29 20:20:23 +0000 2013" in
      let printer = match Date.To.generate_printer "%a %b %d %T %z %Y" with
        | Some p -> p
        | None -> failwith "could not generate printer" in
      let s = Date.To.string printer d in
      print_endline s;
      () in
    let  s = Date.now () in
    for i = -40 to 40 do
      let s' = Date.advance_by_months s i in
      let s'' = Date.end_of_the_month s' in
      let s''' = Date.Printer.to_default s'' in
      print_endline s'''
    done;
    Lwt.return_unit
end

module T = Make(Duration_unix)(Date_unix)

let _ = Lwt_main.run T.test1
