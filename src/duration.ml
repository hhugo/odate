type ms = int

module type Implem = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val compare : t -> t -> int
  val zero : t
  val div : t -> t -> int * t
  val of_ms : ms -> t
  val to_ms : t -> ms
end

module type Unix = sig
  val sleep : ms -> unit Lwt.t
end

type printer = int -> int -> string

type human_readable = {
  forward : bool;
  ms :int;
  s : int;
  m : int;
  h : int;
  day : int;
  month : int;
  year: int;
}

module type S = sig
  type t
  val zero : t
  val zero_human : human_readable
  val add : t -> t -> t
  val sub : t -> t -> t
  val max : t -> t -> t
  val min : t -> t -> t
  val is_negative : t -> bool
  val is_positive : t -> bool
  val is_instantenous : t -> bool
  val abs : t -> t
  val sleep : t -> unit Lwt.t
  val make : ?forward:bool -> ?ms:int -> ?s:int -> ?m:int -> ?h:int -> ?day:int -> ?week:int -> ?month:int -> ?year:int -> unit -> t
  module From : sig
    val ms : int -> t
    val s : int -> t
    val s_float : float -> t
    val h : int -> t
    val m : int -> t
    val day : int -> t
    val month : int -> t
    val week : int -> t
    val year : int -> t
    val human : human_readable -> t
  end
  module To : sig
    val s : t -> int
    val m : t -> int
    val h : t -> int
    val day : t -> int
    val month : t -> int
    val year : t -> int
    val human : t -> human_readable
    val generate_printer : string -> printer option
    val string : printer -> t -> string
    val default_printer : printer
  end
end

module Make (I : Implem)(U : Unix)  = struct
  include Duration_private
  include I
  let sleep x = U.sleep (I.to_ms x)
  let zero_human = {forward=true;ms=0;s=0;m=0;h=0;day=0;month=0;year=0}
  let max a b = if I.compare a b < 0 then b else a
  let min a b = if I.compare a b < 0 then a else b
  let is_positive d = I.compare d I.zero < 0
  let is_negative d = I.compare d I.zero > 0
  let is_instantenous d = I.compare d I.zero = 0
  let abs d = if is_negative d then I.sub zero d else  d
  let make ?(forward=true) ?ms ?s ?m ?h ?day ?week ?month ?year () =
    let aux acc (v,fact) = match v with
      | None -> acc
      | Some v -> v * fact + acc in
    let r = List.fold_left aux 0 [
        ms, ms_in_s;
        s,ms_in_s;
        m,ms_in_min;
        h,ms_in_hour;
        day,ms_in_day;
        week,ms_in_week;
        month,ms_in_month;
        year,ms_in_year ] in
    let ms = if forward then r else (- r) in
    I.of_ms ms
  module From = struct
    let ms ms = make ~ms ()
    let s s = make ~s ()
    let s_float s = make ~ms:(int_of_float (s *. 1000.)) ()
    let m m = make ~m ()
    let h h = make ~h ()
    let day day = make ~day ()
    let month month = make ~month ()
    let year year = make ~year ()
    let week week = make ~week ()
    let human {ms;s;m;h;day;month;year;forward} = make ~forward ~ms ~s ~m ~h ~day ~month ~year ()
  end

  module To = struct
    let ms = I.to_ms
    let s_f t = float_of_int (I.to_ms t) /. (float_of_int ms_in_s)
    let s t = I.to_ms t / ms_in_s
    let m t = I.to_ms t / ms_in_min
    let h t = I.to_ms t / ms_in_hour
    let day t = I.to_ms t / ms_in_day
    let month t = I.to_ms t / ms_in_month
    let year t = I.to_ms t / ms_in_year
    let human t =
      let aux (t,h) (f,fact) =
        let d,r = I.div t (I.of_ms fact) in
        if d > 0
        then r,f h d
        else t,h
      in
      let t,forward = if is_negative t then  abs t,false else t,true in
      let _,h = List.fold_left aux (t,{zero_human with forward}) [
          (fun t year -> {t with year}) ,ms_in_year;
          (fun t month -> {t with month}),ms_in_month;
          (fun t day -> {t with day}),ms_in_day;
          (fun t h -> {t with h}),ms_in_hour;
          (fun t m -> {t with m}),ms_in_min;
          (fun t s -> {t with s}),ms_in_s;
          (fun t ms -> {t with ms}),ms_in_ms; ]
      in h

    let generate_printer s =
      try
        let lexbuf = Lexing.from_string s in
        Some(Duration_parser.main Duration_lexer.tokens lexbuf)
      with exc ->
        Printf.eprintf "exc: %s" (Printexc.to_string exc);
        None

    let generate_or_exit s = match generate_printer s with
      | Some p -> p
      | None -> failwith "could not generate printer"
    let default_printer =
      let fmt =
        "[%>:[%D:[#=1:tomorrow :in ]]]" ^
        "[%Y:[#>0:# year[#>1:s] ][#=0:" ^
        "[%M:[#>0:# month[#>1:s] ][#=0:" ^
        "[%D:[#>1:# day[#>1:s] ][#=0:" ^  (* we don't print days for #=1, because that was taken care with tomorrow/yesterday *)
        "[%h:[#>0:# hour[#>1:s] ][#=0:" ^
        "[%m:[#>0:# minute[#>1:s] ][#=0:" ^
        "[%s:[#>0:# second[#>1:s] :now ]" ^
        "]]]]]]]]]]]" ^
        "[%<:[%D:[#=1:yesterday :ago ]]]" in
      generate_or_exit fmt

    let string (printer : printer) t = printer (I.to_ms t) 0
  end
end
