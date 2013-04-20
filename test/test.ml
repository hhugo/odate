

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
      let printer = match Date.To.generate_printer "%a %b %d %T %z %Y" with
        | Some p -> p
        | None -> failwith "could not generate printer" in
      let s = Date.To.string printer d in
      print_endline s;
      () in
    Lwt.return_unit
end

module T = Make(Duration_unix)(Date_unix)

let _ = Lwt_main.run T.test1
