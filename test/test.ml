

module Make (Duration : Duration.S) (Date : Date.S with type d = Duration.t) = struct
  open Lwt
  let test1 =
    let a = Date.now () in
    let d1 = Duration.From.ms 2999 in
    Duration.sleep d1  >>= fun () ->
    let b = Date.now () in
    let d2 = Date.between a b in
    let s1 = Duration.To.string Duration.To.default_printer d2 in
    let s2 = Duration.To.string Duration.To.default_printer d1 in
    let _ = print_endline s1 in
    let _ = print_endline s2 in
    Lwt.return_unit
end

module T = Make(Duration_unix)(Date_unix)

let _ = Lwt_main.run T.test1
