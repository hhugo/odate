module Implem : Date.Implem = struct
  type t = int

  let compare = Pervasives.compare

  let add = (+)
  let from_seconds x = x
  let to_seconds x = x

  let one_hour = 3600

  let timezone_s ?dst () =
    let open Unix in
    let t,dst = match dst with
      | None -> time (),false
      | Some t -> float_of_int t,true in
    let x = gmtime t in
    let y = localtime t in
    let x',_ = mktime x in
    let y',{tm_isdst} = mktime y in
    let i = int_of_float (y' -. x') in
    if tm_isdst && not dst
    then i - one_hour
    else i

  let get_std_timezone () = timezone_s ()
  let get_dst_timezone dst = timezone_s ~dst ()
  let to_human ?(tz=0) t =
    let open Unix in
    let tm = gmtime (float_of_int (t +  tz)) in
    {
      Date.s = tm.tm_sec;	(*	Seconds 0..60	*)
  	  m = tm.tm_min;	(*	Minutes 0..59	*)
  	  h = tm.tm_hour;	(*	Hours 0..23	*)
  	  day = tm.tm_mday;	(*	Day of month 1..31	*)
  	  month = Date.Month.of_int (tm.tm_mon + 1);	(*	Month of year 0..11	*)
  	  year = tm.tm_year + 1900;	(*	Year - 1900	*)
  	  wday = Date.Weekday.of_int (tm.tm_wday);	(*	Day of week (Sunday is 0)	*)
      tz;
    }
  let now () = int_of_float (Unix.time ())
  let now_milliseconds () = Unix.gettimeofday () *. 1000.
  let from_human ?tz {Date.s;m;h;day;month;year;tz=tz'} =
    let tz = match tz with
      | None -> tz'
      | Some tz -> tz in
    let tm = {
      Unix.tm_sec = s;
      tm_min = m;
      tm_hour = h;
      tm_mday = day;
      tm_mon = (Date.Month.to_int month - 1);
      tm_year = year - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    } in
    let tf,_ = Unix.mktime tm in
    let t = int_of_float tf in
    let offset = get_dst_timezone t - tz in
    offset + t
end

module M = Date.Make(Implem)(Duration_unix)
include M

