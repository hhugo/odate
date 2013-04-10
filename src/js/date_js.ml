
module Implem : Date.Implem = struct
  type t = int64
  let compare = Int64.compare
  let add i x = Int64.add i (Int64.of_int x)
  let from_seconds x = Int64.of_int x
  let to_seconds x = Int64.to_int x

  let diff = ref 0.

  let get_dst_timezone dst =
    let t = jsnew Js.date_fromTimeValue (Int64.to_float (Int64.mul dst 1000L)) in
    -(t##getTimezoneOffset() * 60)
  let get_std_timezone () =
    let jan = jsnew Js.date_day (1998,0,1) in
    let jul = jsnew Js.date_day (1998,6,1) in
    let m = max (jan##getTimezoneOffset()) (jul##getTimezoneOffset()) in
    (- m * 60)


  let to_human ?tz time =
    let ctz = get_dst_timezone time in
    let tz = match tz with
      | None -> ctz
      | Some tz -> tz in
    let tz_offset = tz - ctz in
    let t = jsnew Js.date_fromTimeValue
        (Int64.(to_float
             (mul
                (add (of_int tz_offset) time)
                1000L
             )
         ))
    in
    let h = {
      Date.s=t##getSeconds();
      m=t##getMinutes();
      h=t##getHours();
      day=t##getDate();
      month= Date.Month.of_int (t##getMonth() + 1);
      year = t##getFullYear();
      wday = Date.Weekday.of_int (t##getDay());
      tz
    } in
    h

  let from_human ?tz h =
    let open Date in

    let tz = match tz with
      | None -> h.tz
      | Some tz -> tz in

    let t = jsnew Js.date_sec (h.year,Month.to_int h.month - 1,h.day,h.h,h.m,h.s) in
    let t = Int64.of_float (Js.to_float (t##getTime()) /. 1000.) in
    let offset = get_dst_timezone t - tz in
    Int64.add (Int64.of_int offset) t

  let now () = Int64.of_float (((Js.to_float ((jsnew Js.date_now ())##getTime())) +. !diff) /. 1000.)

  let now_milliseconds () = Js.to_float (jsnew Js.date_now ())##getTime()

  let from_js tjs =
    Int64.of_float (Js.to_float (tjs##getTime()) /. 1000.)

  let to_js t =
    let t = jsnew Js.date_fromTimeValue (Int64.to_float (Int64.mul t 1000L)) in
    t
end
module M = Date.Make(Implem)(Duration_js)
include M
