(** Duration *)

type printer
(** printer *)

type human_readable = {
  forward : bool;  (** {b true} means in the future *)
  ms : int;
  s : int;
  m : int;
  h : int;
  day : int;
  month : int;
  year : int;
}

module type S = sig
  type t

  val zero : t

  val zero_human : human_readable

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val max : t -> t -> t

  val min : t -> t -> t

  val is_negative : t -> bool

  val is_positive : t -> bool

  val is_instantenous : t -> bool

  val abs : t -> t

  val make :
    ?forward:bool ->
    ?ms:int ->
    ?s:int ->
    ?m:int ->
    ?h:int ->
    ?day:int ->
    ?week:int ->
    ?month:int ->
    ?year:int ->
    unit ->
    t

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
    val ms : t -> int

    val s : t -> int

    val s_float : t -> float

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

include S
