type weekday =
  [ `Monday | `Tuesday | `Wednesday | `Thursday | `Friday | `Saturday | `Sunday ]

type month =
  [ `January
  | `February
  | `March
  | `April
  | `May
  | `June
  | `July
  | `August
  | `September
  | `October
  | `November
  | `December ]

type year = int

type day = int

type tz_internal = int

type tz = UTC | Local | Plus of tz_internal

type human_readable = {
  s : int;
  m : int;
  h : int;
  day : day;
  wday : weekday;
  month : month;
  year : year;
  tz : tz;
}

type pointer = { mutable pos : int; str : string; length : int }

type printer = human_readable -> string

type format = string

type parser_ = pointer -> human_readable -> human_readable

module type Clock = sig
  (** Clock operations.
      Currently read-only to retrieve the time in various formats. *)
  type tm = {
    tm_sec : int;  (** Seconds 0..60 *)
    tm_min : int;  (** Minutes 0..59 *)
    tm_hour : int;  (** Hours 0..23 *)
    tm_mday : int;  (** Day of month 1..31 *)
    tm_mon : int;  (** Month of year 0..11 *)
    tm_year : int;  (** Year - 1900 *)
    tm_wday : int;  (** Day of week (Sunday is 0) *)
    tm_yday : int;  (** Day of year 0..365 *)
    tm_isdst : bool;  (** Daylight time savings in effect *)
  }
  (** The type representing wallclock time and calendar date. *)

  val time : unit -> float
  (** Return the current time since 00:00:00 GMT, Jan. 1, 1970, in
      seconds. *)

  val gettimeofday : unit -> float
  (** Same as time, but with resolution better than 1 second. *)

  val gmtime : float -> tm
  (** Convert a time in seconds, as returned by {!time}, into a
      date and a time. Assumes UTC (Coordinated Universal Time), also
      known as GMT. *)

  val localtime : float -> tm
  (** Convert a time in seconds, as returned by {!Unix.time}, into a
      date and a time. Assumes the local time zone. *)

  val mktime : tm -> float * tm
  (** Convert a date and time, specified by the tm argument, into a time
      in seconds, as returned by Unix.time.
      The tm_isdst, tm_wday and tm_yday fields of tm are ignored.
      Also return a normalized copy of the given tm record, with the
      tm_wday, tm_yday, and tm_isdst fields recomputed from the other
      fields, and the other fields normalized (so that, e.g., 40 October
      is changed into 9 November). The tm argument is interpreted in
      the local time zone. *)
end

module type Implem = sig
  type t

  val compare : t -> t -> int

  val to_human : ?tz:tz -> t -> human_readable

  val from_human : ?tz:tz -> human_readable -> t

  val now : unit -> t

  val now_milliseconds : unit -> float

  val get_std_timezone : unit -> tz_internal

  val get_dst_timezone : t -> tz_internal

  val add : t -> float -> t

  val from_seconds : float -> t

  val to_seconds : t -> float
end

module type S = sig
  type t

  type d = ODuration.t

  val beginning_of_the_day : ?tz:tz -> t -> t

  val beginning_of_the_month : ?tz:tz -> t -> t

  val beginning_of_the_week : ?tz:tz -> t -> t

  val end_of_the_day : ?tz:tz -> t -> t

  val end_of_the_month : ?tz:tz -> t -> t

  val end_of_the_week : ?tz:tz -> t -> t

  val gmt : tz

  val compare : t -> t -> int

  val empty : human_readable

  val some_if_valid : t -> t option

  val now : unit -> t

  val now_milliseconds : unit -> float

  val get_std_timezone : unit -> tz_internal

  val get_dst_timezone : t -> tz_internal

  val epoch : t

  val make :
    ?tz:tz ->
    ?s:int ->
    ?m:int ->
    ?h:int ->
    day:int ->
    month:month ->
    year:int ->
    unit ->
    t

  (* make date in gmt *)
  val move : t -> d -> t

  val advance_by_minutes : t -> int -> t

  val advance_by_hours : t -> int -> t

  val advance_by_days : t -> int -> t

  val advance_by_months : t -> int -> t

  val advance_by_years : t -> int -> t

  val convert_with_tz : tz -> tz -> t -> t

  val advance_by_weeks : t -> int -> t

  val move_to_weekday : ?tz:tz -> t -> forward:bool -> weekday -> t

  val calendar_advance : t -> ODuration.human_readable -> t

  val between : t -> t -> d

  val in_between : t -> t -> t

  val max : t -> t -> t

  val min : t -> t -> t

  val is_after : t -> t -> bool

  val is_before : t -> t -> bool

  val is_epoch : t -> bool

  val get_age : t -> int

  val get_weekday : ?tz:tz -> t -> weekday

  val get_day : ?tz:tz -> t -> int

  val get_month : ?tz:tz -> t -> month

  val get_year : ?tz:tz -> t -> year

  val get_first_week : ?tz:tz -> year -> t

  val get_week_number : ?tz:tz -> t -> int

  val get_min : ?tz:tz -> t -> int

  val get_hour : ?tz:tz -> t -> int

  val get_sec : ?tz:tz -> t -> int

  module Format : sig
    val default : string

    val debug : string

    val birthday : string

    val iso : string
  end

  module Printer : sig
    val combine : printer -> printer -> printer

    val default : printer

    val debug : printer

    val birthday : printer

    val iso : printer

    val to_default : t -> string

    val to_debug : t -> string

    val to_birthday : t -> string

    val to_iso : t -> string
  end

  module Parser : sig
    val combine : parser_ -> parser_ -> parser_

    val sum : parser_ list -> parser_

    val twitter : parser_

    val runkeeper : parser_

    val nike : parser_

    val birthday : parser_

    val from_nike : string -> t

    val from_runkeeper : string -> t

    val from_twitter : string -> t

    val from_birthday : string -> t

    val from_iso : string -> t
  end

  module From : sig
    val seconds : int -> t

    val seconds_float : float -> t

    val generate_parser : string -> parser_ option

    val string : parser_ -> string -> t

    val human : ?tz:tz -> human_readable -> t
  end

  module To : sig
    val seconds : t -> int

    val seconds_float : t -> float

    val generate_printer : string -> printer option

    val string : ?tz:tz -> printer -> t -> string

    val human : ?tz:tz -> t -> human_readable
  end
end

let date_token s =
  let lexbuf = Lexing.from_string s in
  let open Date_lexer in
  let rec loop acc =
    match Date_lexer.tokens lexbuf with
    | EOL -> List.rev acc
    | x -> loop (x :: acc)
  in
  loop []

module Hour = struct
  let is_am h = h < 12

  let convert_24h_to_12h h = if h > 12 then h - 12 else h
end

module Weekday = struct
  let of_int = function
    | 0 -> `Sunday
    | 1 -> `Monday
    | 2 -> `Tuesday
    | 3 -> `Wednesday
    | 4 -> `Thursday
    | 5 -> `Friday
    | 6 -> `Saturday
    | _ -> failwith "Weekday.of_int"

  let to_int = function
    | `Sunday -> 0
    | `Monday -> 1
    | `Tuesday -> 2
    | `Wednesday -> 3
    | `Thursday -> 4
    | `Friday -> 5
    | `Saturday -> 6

  let stringifier =
    ref (function
      | `Sunday -> "Sunday"
      | `Monday -> "Monday"
      | `Tuesday -> "Tuesday"
      | `Wednesday -> "Wednesday"
      | `Thursday -> "Thursday"
      | `Friday -> "Friday"
      | `Saturday -> "Saturday")

  let set_stringifier f = stringifier := f

  let to_string x = !stringifier x

  let stringifier_abbr =
    ref (function
      | `Sunday -> "Sun"
      | `Monday -> "Mon"
      | `Tuesday -> "Tue"
      | `Wednesday -> "Wed"
      | `Thursday -> "Thu"
      | `Friday -> "Fri"
      | `Saturday -> "Sat")

  let set_stringifier_abbr f = stringifier_abbr := f

  let to_string_short x = !stringifier_abbr x
end

module Month = struct
  let of_int = function
    | 1 -> `January
    | 2 -> `February
    | 3 -> `March
    | 4 -> `April
    | 5 -> `May
    | 6 -> `June
    | 7 -> `July
    | 8 -> `August
    | 9 -> `September
    | 10 -> `October
    | 11 -> `November
    | 12 -> `December
    | _ -> failwith "Month.of_int"

  let to_int = function
    | `January -> 1
    | `February -> 2
    | `March -> 3
    | `April -> 4
    | `May -> 5
    | `June -> 6
    | `July -> 7
    | `August -> 8
    | `September -> 9
    | `October -> 10
    | `November -> 11
    | `December -> 12

  let stringifier =
    ref (function
      | `January -> "January"
      | `February -> "February"
      | `March -> "March"
      | `April -> "April"
      | `May -> "May"
      | `June -> "June"
      | `July -> "July"
      | `August -> "August"
      | `September -> "September"
      | `October -> "October"
      | `November -> "November"
      | `December -> "December")

  let set_stringifier f = stringifier := f

  let to_string x = !stringifier x

  let stringifier_abbr =
    ref (function
      | `January -> "Jan"
      | `February -> "Feb"
      | `March -> "Mar"
      | `April -> "Apr"
      | `May -> "May"
      | `June -> "Jun"
      | `July -> "Jul"
      | `August -> "Aug"
      | `September -> "Sep"
      | `October -> "Oct"
      | `November -> "Nov"
      | `December -> "Dec")

  let set_stringifier_abbr f = stringifier_abbr := f

  let to_string_short x = !stringifier_abbr x

  let next m =
    let i = to_int m in
    of_int (if i = 12 then 1 else i + 1)

  let prev m =
    let i = to_int m in
    of_int (if i = 1 then 12 else i - 1)
end

module Make (Implem : Implem) = struct
  include Implem
  module D = ODuration

  type d = D.t

  let gmt = UTC

  let now () = Implem.now ()

  let empty =
    {
      s = 0;
      m = 0;
      h = 0;
      day = 0;
      wday = `Monday;
      month = `January;
      year = 0;
      tz = UTC;
    }

  let epoch = Implem.from_seconds 0.

  let some_if_valid t = if epoch = t then None else Some t

  let make ?tz ?(s = 0) ?(m = 0) ?(h = 0) ~day ~month ~year () =
    Implem.from_human ?tz
      { s; m; h; day; year; month; wday = `Monday; tz = UTC }

  let move t time = Implem.add t (D.To.s_float time)

  let abbreviations =
    [
      ('c', "%a %b %_d %Y %H:%M:%S");
      ('e', "%_d");
      ('D', "%m/%d/%y");
      ('F', "%Y-%m-%d");
      ('h', "%b");
      ('R', "%H:%M");
      ('T', "%H:%M:%S");
    ]

  module TimeZone = struct
    let s : t -> tz -> int =
     fun t -> function UTC -> 0 | Plus n -> n | Local -> get_dst_timezone t
  end

  module From = struct
    let make_state str = { pos = 0; str; length = String.length str }

    let _parse_constant s =
      let size = String.length s in
      fun ptr ->
        let { str; pos; _ } = ptr in
        let rec loop i =
          if i = size then ()
          else if s.[i] = str.[i + pos] then loop (i + 1)
          else failwith ""
        in
        let () = loop 0 in
        ptr.pos <- ptr.pos + size

    let parse_constant_i s =
      let ( +++ ) a i = char_of_int (int_of_char a + i) in
      let eq c1 c2 =
        c1 = c2
        || (c1 >= 'a' && c1 <= 'z' && c1 = c2 +++ 32)
        || (c2 >= 'a' && c2 <= 'z' && c2 = c1 +++ 32)
      in
      let size = String.length s in
      fun ptr ->
        let { str; pos; _ } = ptr in
        let rec loop i =
          if i = size then ()
          else if eq s.[i] str.[i + pos] then loop (i + 1)
          else failwith ""
        in
        let () = loop 0 in
        ptr.pos <- ptr.pos + size

    let parse_sum l ptr =
      let pos = ptr.pos in
      let rec loop = function
        | [] -> failwith "Sum"
        | [ f ] ->
            ptr.pos <- pos;
            f ptr
        | f :: fs -> (
            try
              ptr.pos <- pos;
              f ptr
            with _ -> loop fs )
      in
      loop l

    let parse_num ?max ?(min = 1) =
      let check =
        match max with
        | Some ma -> fun l c -> l < ma && c >= '0' && c <= '9'
        | None -> fun _ c -> c >= '0' && c <= '9'
      in
      let rec loop endpos pos l str =
        if pos < endpos && check l str.[pos] then
          loop endpos (pos + 1) (l + 1) str
        else l
      in
      fun ptr ->
        let l = loop ptr.length ptr.pos 0 ptr.str in
        if l < min then failwith "too small"
        else
          let d = int_of_string (String.sub ptr.str ptr.pos l) in
          ptr.pos <- ptr.pos + l;
          d

    let parse_num_size size = parse_num ~max:size ~min:size

    let parse_char ?max ?min char =
      let check =
        match max with
        | Some ma -> fun l c -> l < ma && char = c
        | None -> fun _ c -> char = c
      in
      let rec loop endpos pos l str =
        if pos < endpos && check l str.[pos] then
          loop endpos (pos + 1) (l + 1) str
        else
          match min with
          | Some min when min <= l -> l
          | None -> l
          | _ -> failwith ""
      in
      fun ptr ->
        let l = loop ptr.length ptr.pos 0 ptr.str in
        ptr.pos <- ptr.pos + l;
        l

    let parse_single_char = parse_char ~min:1 ~max:1

    let _parse_char_plus x = parse_char ~min:1 x

    let ignore' f ptr = ignore (f ptr)

    let ( >>== ) f g ptr = g (f ptr)

    let ( >>=| ) f c ptr =
      f ptr;
      c

    let _parse_remaining ptr =
      let size = String.length ptr.str in
      let s = String.sub ptr.str ptr.pos (size - ptr.pos) in
      ptr.pos <- size;
      s

    let parse_option p ptr = try Some (p ptr) with _ -> None

    let parse_weekday short =
      let rec loop i acc =
        if i = 7 then acc
        else
          let w = Weekday.of_int i in
          let s = Weekday.to_string w in
          let s = if short then String.sub s 0 3 else s in
          loop (succ i) ((parse_constant_i s >>=| w) :: acc)
      in
      parse_sum (loop 0 [ (fun _ptr -> failwith "weekday") ])

    let parse_month short =
      let rec loop i acc =
        if i = 13 then acc
        else
          let m = Month.of_int i in
          let s = Month.to_string m in
          let s = if short then String.sub s 0 3 else s in
          loop (succ i) ((parse_constant_i s >>=| m) :: acc)
      in
      parse_sum (loop 1 [])

    let _parse_seq l ptr =
      let rec loop acc = function
        | [] -> List.rev acc
        | x :: xs -> loop (x ptr :: acc) xs
      in
      loop [] l

    let _parse_fold f acc l ptr =
      let rec loop acc = function
        | [] -> acc
        | x :: xs -> loop (f acc (x ptr)) xs
      in
      loop acc l

    let parse_sign =
      parse_sum
        [
          ignore' (parse_single_char '-') >>=| ( - );
          ignore' (parse_single_char '+') >>=| ( + );
        ]

    let parse_timezone =
      let gmt = parse_constant_i "gmt" >>== fun () -> UTC in
      let p2 = parse_num_size 2 in
      let other ptr =
        let s = parse_sign ptr in
        let hh = p2 ptr in
        let _ = parse_option (parse_single_char ':') ptr in
        let mm = parse_option p2 ptr in
        let mm =
          match mm with
          | None -> 0
          | Some mm ->
              let _ = parse_option (parse_single_char ':') ptr in
              let _ = parse_option p2 ptr in
              mm
        in
        match s 0 ((hh * 60) + mm) with 0 -> UTC | n -> Plus n
      in

      parse_sum [ other; gmt ]

    let seconds x = Implem.from_seconds (float_of_int x)

    let seconds_float x = Implem.from_seconds x

    let parse_pm =
      parse_sum
        [ parse_constant_i "am" >>=| false; parse_constant_i "pm" >>=| true ]

    let update_pm pm d =
      let h =
        if pm then if d.h = 12 then 12 else d.h + 12
        else if d.h = 12 then 0
        else d.h
      in
      { d with h }

    let parse_directive, add_abbr =
      let l =
        ref
          [
            ('a', parse_weekday true >>== fun wday d -> { d with wday });
            ('A', parse_weekday false >>== fun wday d -> { d with wday });
            ('b', parse_month true >>== fun month d -> { d with month });
            ('B', parse_month false >>== fun month d -> { d with month });
            ('d', parse_num ~max:2 >>== fun day d -> { d with day });
            ('H', parse_num ~max:2 >>== fun h d -> { d with h });
            ('I', parse_num ~max:2 >>== fun h d -> { d with h });
            ('k', parse_num ~max:2 >>== fun h d -> { d with h });
            ('l', parse_num ~max:2 >>== fun h d -> { d with h });
            ( 'm',
              parse_num ~max:2 >>== fun month ->
              let month = Month.of_int month in
              fun d -> { d with month } );
            ('M', parse_num ~max:2 >>== fun m d -> { d with m });
            ('p', parse_pm >>== update_pm);
            ('P', parse_pm >>== update_pm);
            ('S', parse_num ~max:2 >>== fun s d -> { d with s });
            ( 'u',
              parse_num >>== fun wday ->
              let wday = Weekday.of_int (wday - 1) in
              fun d -> { d with wday } );
            ( 'w',
              parse_num >>== fun wday ->
              let wday = Weekday.of_int wday in
              fun d -> { d with wday } );
            ( 'y',
              parse_num ~max:2 >>== fun y ->
              let year = if y < 70 then 2000 + y else 1900 + y in
              fun d -> { d with year } );
            ('Y', parse_num ~max:4 >>== fun year d -> { d with year });
            ('z', parse_timezone >>== fun tz d -> { d with tz });
          ]
      in
      ( (fun (_, d) -> List.assoc d !l),
        fun abbr ->
          let l' = !l in
          l := abbr :: l' )

    let parse_space =
      let l = List.map (fun c -> parse_char ~min:1 c) [ ' '; '\n'; '\t' ] in
      parse_sum l

    let generate_parser s =
      let open Date_lexer in
      try
        let tokens = date_token s in
        let l =
          List.rev_map
            (function
              | String s -> parse_constant_i s >>=| fun d -> d
              | Space -> ignore' parse_space >>=| fun d -> d
              | Directive (p, c) -> parse_directive (p, c)
              | EOL -> assert false)
            tokens
        in
        let rec loop f = function
          | [] -> f
          | x :: xs ->
              loop
                (fun ptr d' ->
                  let d = x ptr d' in
                  f ptr d)
                xs
        in
        let parser_ = loop (fun _ d -> d) l in
        Some (fun ptr d -> parser_ ptr d)
      with _e -> (*        print_endline (Printexc.to_string _e); *)
                 None

    let _ =
      let abbr =
        List.rev_map
          (fun (c, abbr) ->
            match generate_parser abbr with
            | Some p -> (c, fun ptr d -> p ptr d)
            | None ->
                failwith
                  (Printf.sprintf "could not generate_abbreviations for %c" c))
          abbreviations
      in
      List.iter add_abbr abbr

    let human ?tz { s; m; h; day; month; year; tz = tz'; wday = _ } =
      let tz = match tz with None -> tz' | Some x -> x in
      make ~tz ~s ~m ~h ~day ~month ~year ()

    let string (parser_ : parser_) (s : string) =
      let ptr = make_state s in
      human (parser_ ptr empty)
  end

  module To = struct
    let seconds t = int_of_float (Implem.to_seconds t)

    let seconds_float t = Implem.to_seconds t

    let fill s size c =
      let n = size - String.length s in
      if n > 0 then String.make n c ^ s else s

    let pad (i, padopt, paddefault, size) =
      let padi =
        match padopt with
        | `None -> Some paddefault
        | `Pad c -> Some c
        | `NoPad | `Upper | `Tz _ -> None
      in
      let s = string_of_int i in
      match padi with None -> s | Some padi -> fill s size padi

    let parse_directive, add_abbr =
      let l =
        ref
          [
            ('a', false, fun (_, d) -> String.sub (Weekday.to_string d.wday) 0 3);
            ('A', false, fun (_, d) -> Weekday.to_string d.wday);
            ('b', false, fun (_, d) -> String.sub (Month.to_string d.month) 0 3);
            ('B', false, fun (_, d) -> Month.to_string d.month);
            ('C', true, fun (p, d) -> pad (d.year / 100, p, '_', 2));
            ('d', true, fun (p, d) -> pad (d.day, p, '0', 2));
            ( 'E',
              false,
              fun (_, d) ->
                match d.day with
                | 1 -> "1st"
                | 2 -> "2nd"
                | 3 -> "3rd"
                | x -> string_of_int x ^ "th" );
            ('H', true, fun (p, d) -> pad (d.h, p, '0', 2));
            ( 'I',
              true,
              fun (p, d) -> pad (Hour.convert_24h_to_12h d.h, p, '0', 2) );
            ('k', true, fun (p, d) -> pad (d.h, p, '_', 2));
            ( 'l',
              true,
              fun (p, d) -> pad (Hour.convert_24h_to_12h d.h, p, '_', 2) );
            ('m', true, fun (p, d) -> pad (Month.to_int d.month, p, '0', 2));
            ('M', true, fun (p, d) -> pad (d.m, p, '0', 2));
            ('p', false, fun (_, d) -> if Hour.is_am d.h then "AM" else "PM");
            ('P', false, fun (_, d) -> if Hour.is_am d.h then "am" else "pm");
            ('S', true, fun (p, d) -> pad (d.s, p, '0', 2));
            ( 'u',
              false,
              fun (_, d) ->
                let wd = Weekday.to_int d.wday in
                string_of_int (if wd == 0 then 7 else wd) );
            ('w', false, fun (_, d) -> string_of_int (Weekday.to_int d.wday));
            ('y', false, fun (_, d) -> pad (d.year mod 100, `Pad '0', '0', 2));
            ('Y', true, fun (p, d) -> pad (d.year, p, '_', 4));
            ( 'z',
              true,
              fun (p, d) ->
                let tz = TimeZone.s (From.human d) d.tz in
                let sign, tz = if tz < 0 then ("-", -tz) else ("+", tz) in
                let min = tz / 60 in
                let sec = tz mod 60 in
                let hour = pad (min / 60, `Pad '0', '0', 2) in
                let min = pad (min mod 60, `Pad '0', '0', 2) in
                let sec = pad (sec, `Pad '0', '0', 2) in
                match p with
                | `Tz 1 -> sign ^ hour ^ ":" ^ min
                | `Tz 2 -> sign ^ hour ^ ":" ^ min ^ ":" ^ sec
                | `Tz 3 -> sign ^ hour
                | `Tz _ | `Upper | `NoPad | `Pad _ | `None -> sign ^ hour ^ min
            );
          ]
      in
      let rec assoc c = function
        | (c', a, b) :: _ when c = c' -> (a, b)
        | _ :: xs -> assoc c xs
        | _ -> raise Not_found
      in
      ( (fun c -> assoc c !l),
        fun abbr ->
          let l' = !l in
          l := abbr :: l' )

    let human ?tz t = Implem.to_human ?tz t

    let generate_printer s =
      let open Date_lexer in
      try
        let tokens = date_token s in
        let l =
          List.map
            (function
              | String s -> fun buf _ -> Buffer.add_string buf s
              | Space -> fun buf _ -> Buffer.add_char buf ' '
              | Directive (p, c) -> (
                  let allow, f = parse_directive c in
                  match (allow, p) with
                  | false, `Pad _ -> failwith "not padding here"
                  | _ ->
                      fun buf d ->
                        let s = f (p, d) in
                        Buffer.add_string buf s )
              | EOL -> assert false)
            tokens
        in
        Some
          (fun h ->
            let buf = Buffer.create 0 in
            List.iter (fun f -> f buf h) l;
            Buffer.contents buf)
      with _ -> None

    let _ =
      let abbr =
        List.rev_map
          (fun (c, abbr) ->
            match generate_printer abbr with
            | Some p -> (c, false, fun (_, d) -> p d)
            | None -> failwith "could not generate_abbreviations")
          abbreviations
      in
      List.iter add_abbr abbr

    let string ?tz format t =
      let h = human ?tz t in
      format h
  end

  let between t1 t2 =
    D.( - )
      (D.From.s_float (Implem.to_seconds t2))
      (D.From.s_float (Implem.to_seconds t1))

  let in_between t1 t2 =
    let d = D.To.s_float (between t1 t2) /. 2. in
    Implem.add t1 d

  let get_year ?tz t = (To.human ?tz t).year

  let get_month ?tz t = (To.human ?tz t).month

  let get_day ?tz t = (To.human ?tz t).day

  let get_weekday ?tz t = (To.human ?tz t).wday

  let get_sec ?tz t = (To.human ?tz t).s

  let get_min ?tz t = (To.human ?tz t).m

  let get_hour ?tz t = (To.human ?tz t).h

  let get_std_timezone () : tz_internal = Implem.get_std_timezone ()

  let get_dst_timezone dst : tz_internal = Implem.get_dst_timezone dst

  let mod_normalized a n =
    let a = a mod n in
    if a < 0 then a + n else a

  let calendar_advance t { D.forward; h; m; s; day; month; year; ms = _ } =
    let human = To.human t in
    let add = if forward then fun a x -> a + x else fun a x -> a - x in
    let human =
      {
        human with
        h = add human.h h;
        m = add human.m m;
        s = add human.s s;
        day = add human.day day;
        year = add human.year year;
      }
    in
    let m = add (Month.to_int human.month) month in
    let dy =
      let dd = (m - 1) / 12 in
      if m < 1 then dd - 1 else dd
    in
    let month = if m > 12 || m < 1 then mod_normalized (m - 1) 12 + 1 else m in
    let month = Month.of_int month in
    let human = { human with month; year = human.year + dy } in
    From.human human

  let advance_by_minutes t h = move t (D.From.m h)

  let advance_by_hours t h = move t (D.From.h h)

  let advance_by_days t day = calendar_advance t { D.zero_human with D.day }

  let advance_by_weeks t n =
    calendar_advance t { D.zero_human with D.day = 7 * n }

  let advance_by_months t month =
    calendar_advance t { D.zero_human with D.month }

  let advance_by_years t year = calendar_advance t { D.zero_human with D.year }

  let convert_with_tz f_tz t_tz t =
    let f_tz = TimeZone.s t f_tz in
    let t_tz = TimeZone.s t t_tz in
    let dtz = t_tz - f_tz in
    let h = To.human ~tz:UTC t in
    From.human { h with tz = Plus dtz }

  let move_to_weekday ?tz t ~forward wd =
    let dir = if forward then 1 else -1 in
    let rec shift t =
      let d = To.human ?tz t in
      if d.wday = wd then t else shift (advance_by_days t dir)
    in
    shift t

  let get_first_week ?tz year =
    let t = make ?tz ~day:1 ~month:`January ~year () in
    let t = move_to_weekday ?tz t ~forward:true `Thursday in
    let t = move_to_weekday ?tz t ~forward:false `Monday in
    t

  let beginning_of_the_day ?tz date =
    let hum = to_human ?tz date in
    From.human { hum with h = 0; m = 0; s = 0 }

  let end_of_the_day ?tz date =
    let hum = to_human ?tz date in
    From.human { hum with h = 23; m = 59; s = 60 }

  let beginning_of_the_week ?tz date =
    (* let wd = Date.get_weekday date in *)
    (* match wd with *)
    (*   | `Sunday -> Date.move_to_weekday date ~forward:true `Monday *)
    (*   | _ ->  *)
    move_to_weekday ?tz date ~forward:false `Monday

  let end_of_the_week ?tz date = move_to_weekday ?tz date ~forward:true `Sunday

  let end_of_the_month ?tz date =
    let rec loop hum date =
      let day = if hum.day < 28 then 28 - hum.day else 1 in
      let date' = advance_by_days date day in
      let hum' = To.human ?tz date' in
      if hum.month = hum'.month then loop hum' date' else date
    in
    loop (To.human ?tz date) date

  let beginning_of_the_month ?tz date =
    let hum = To.human ?tz date in
    From.human ?tz { hum with day = 1 }

  let get_week_number ?tz t =
    let y = get_year ?tz t in
    let t1 = get_first_week y in
    let d = between t1 t in
    let d = D.To.day d in
    (d / 7) + 1

  let max a b = max a b

  let min a b = min a b

  let is_after t1 t2 = t1 > t2

  let is_before t1 t2 = t1 < t2

  let is_epoch t = t = epoch

  let get_age d =
    let now = now () in
    let now = convert_with_tz Local gmt now in
    let hn = To.human now in
    let h = To.human d in
    let h' = { h with year = hn.year } in
    let d' = From.human h' in
    let y = hn.year - h.year - if d' < now then 0 else 1 in
    y

  module Format = struct
    let default = "%c"

    let debug = "%Y-%m-%d | %H:%M:%S"

    let birthday = "%m/%d/%Y"

    let iso = "%FT%TZ"
  end

  module Printer = struct
    let combine p1 p2 d =
      let s1 = p1 d and s2 = p2 d in
      s1 ^ s2

    let generate_or_exit s =
      match To.generate_printer s with
      | Some p -> p
      | None -> failwith "could not generate printer"

    let default = generate_or_exit Format.default

    let debug = generate_or_exit Format.debug

    let birthday = generate_or_exit Format.birthday

    let iso = generate_or_exit Format.iso

    let to_default = To.string default

    let to_debug = To.string debug

    let to_birthday = To.string birthday

    let to_iso = To.string ~tz:gmt iso
  end

  module Parser = struct
    let combine p1 p2 ptr d =
      let d = p1 ptr d in
      p2 ptr d

    let sum = From.parse_sum

    let generate_or_exit s =
      match From.generate_parser s with
      | Some p -> p
      | None -> failwith "could not generate parser"

    let twitter = generate_or_exit "%a %b %d %T %z %Y"

    let runkeeper = generate_or_exit "%a, %d %b %Y %H:%M:%S"

    let nike = generate_or_exit "%FT%Tz"

    let birthday = generate_or_exit Format.birthday

    let iso = generate_or_exit Format.iso

    let from_nike = From.string nike

    let from_runkeeper = From.string runkeeper

    let from_twitter = From.string twitter

    let from_birthday = From.string birthday

    let from_iso = From.string iso
  end
end

module MakeImplem (C : Clock) : Implem = struct
  type t = float

  let compare = Stdlib.compare

  let add f i = f +. i

  let from_seconds x = x

  let to_seconds x = x

  let one_hour = 3600.

  let timezone_s ?dst () =
    let t, dst =
      match dst with None -> (C.time (), false) | Some t -> (t, true)
    in
    let x = C.gmtime t in
    let y = C.localtime t in
    let x', _ = C.mktime x in
    let y', { C.tm_isdst; _ } = C.mktime y in
    let i = y' -. x' in
    if tm_isdst && not dst then int_of_float (i -. one_hour) else int_of_float i

  let get_std_timezone () = timezone_s ()

  let get_dst_timezone dst = timezone_s ~dst ()

  let to_human ?(tz = UTC) t =
    let tm =
      match tz with
      | UTC -> C.gmtime t
      | Local -> C.localtime t
      | Plus tz' -> C.gmtime (t +. float_of_int tz')
    in
    {
      s = tm.C.tm_sec;
      (*	Seconds 0..60	*)
      m = tm.C.tm_min;
      (*	Minutes 0..59	*)
      h = tm.C.tm_hour;
      (*	Hours 0..23	*)
      day = tm.C.tm_mday;
      (*	Day of month 1..31	*)
      month = Month.of_int (tm.C.tm_mon + 1);
      (*	Month of year 0..11	*)
      year = tm.C.tm_year + 1900;
      (*	Year - 1900	*)
      wday = Weekday.of_int tm.C.tm_wday;
      (*	Day of week (Sunday is 0)	*)
      tz;
    }

  let now () = C.time ()

  let now_milliseconds () = C.gettimeofday () *. 1000.

  let from_human ?tz { s; m; h; day; month; year; tz = tz'; wday = _ } =
    let tz = match tz with None -> tz' | Some tz -> tz in
    let tm =
      {
        C.tm_sec = s;
        tm_min = m;
        tm_hour = h;
        tm_mday = day;
        tm_mon = Month.to_int month - 1;
        tm_year = year - 1900;
        tm_wday = 0;
        tm_yday = 0;
        tm_isdst = false;
      }
    in
    let tf, _ = C.mktime tm in
    let t = tf in
    let local_tz : tz_internal = get_dst_timezone t in
    let offset : int =
      match tz with Local -> 0 | UTC -> local_tz | Plus n -> local_tz - n
    in
    t +. float_of_int offset
end

module Unix = Make (MakeImplem (Unix))
