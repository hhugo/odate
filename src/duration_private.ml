module P = Stdlib

module O : sig
  type t

  val ( * ) : t -> t -> t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( / ) : t -> t -> t

  val abs : t -> t

  val of_int : int -> t

  val of_float : float -> t

  val compare : t -> t -> int

  val lt : t -> t -> bool

  val le : t -> t -> bool

  val gt : t -> t -> bool

  val ge : t -> t -> bool

  val to_string : t -> string

  val zero : t

  val one : t

  val to_float : t -> float

  val to_int : t -> int

  val div : t -> t -> int * t
end = struct
  type t = float

  let ( * ) = ( *. )

  let ( + ) = ( +. )

  let ( - ) = ( -. )

  let ( / ) = ( /. )

  let abs = abs_float

  let of_int = float_of_int

  let of_float x = x

  let compare (a : float) (b : float) = compare a b

  let lt a b = compare a b < 0

  let le a b = compare a b <= 0

  let gt a b = compare a b > 0

  let ge a b = compare a b >= 0

  let to_string x = string_of_int (int_of_float x)

  let zero = 0.0

  let one = 1.0

  let to_float x = x

  let to_int x = int_of_float x

  let div duration diviseur =
    let d = duration / diviseur in
    if lt (abs d) one then (0, duration)
    else (int_of_float d, mod_float duration diviseur)
end

open O

let ms_in_ms = of_int 1

let ms_in_s = of_int 1000

let s_in_s = of_int 1

let s_in_min = of_int 60

let ms_in_min = ms_in_s * s_in_min

let min_in_hour = of_int 60

let ms_in_hour = ms_in_min * min_in_hour

let hour_in_day = of_int 24

let ms_in_day = ms_in_hour * hour_in_day

let day_in_week = of_int 7

let ms_in_week = ms_in_day * day_in_week

let day_in_year = of_float 365.24219879

let month_in_year = of_int 12

let day_in_month = day_in_year / month_in_year

let ms_in_month = ms_in_day * day_in_month

let ms_in_year = ms_in_day * day_in_year

let fact_of_directive = function
  | 'x' -> ms_in_ms
  | 's' -> ms_in_s
  | 'm' -> ms_in_min
  | 'h' -> ms_in_hour
  | 'D' -> ms_in_day
  | 'W' -> ms_in_week
  | 'M' -> ms_in_month
  | 'Y' -> ms_in_year
  | _ -> assert false

let apply_directive dir d =
  let units = fact_of_directive dir in
  let dv = d in
  let fwd = ge dv (of_int 0) in
  let update = if fwd then ( - ) else ( + ) in
  let v' = abs dv / units in
  if lt v' one then (d, zero)
  else
    let new_d = update d (v' * units) in
    (new_d, v')

let static_printer s : O.t -> O.t -> string = fun _ _ -> s

let empty_printer = static_printer ""

let show_value (w : int) (_d : O.t) v =
  let str = to_string v in
  let i = P.( - ) w (String.length str) in
  if i > 0 then String.make i '0' ^ str else str

let check_condition cmp int true' false' =
  let false' = match false' with None -> empty_printer | Some y -> y in
  let cmp =
    match cmp with `EQ -> ( = ) | `NEQ -> ( <> ) | `LT -> ( < ) | `GT -> ( > )
  in
  fun d v -> if cmp (to_int v) int then true' d v else false' d v

let check_condition_simple cmp true' =
  let cmp : int -> bool =
    match cmp with
    | `EQ i -> ( = ) i
    | `LT -> fun i -> i < 0
    | `GT -> fun i -> i > 0
  in
  fun (d : O.t) (v : O.t) -> if cmp (O.to_int d) then true' d v else ""

let directive_block (dir : char) expr (d : O.t) _v =
  let nd, nv = apply_directive dir d in
  expr nd nv
