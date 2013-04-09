let sub = (-)
let add = (+)

let ms_in_ms = 1
let ms_in_s = 1000
let s_in_s = 1

let s_in_min = 60
let ms_in_min = ms_in_s * s_in_min

let min_in_hour = 60
let ms_in_hour = ms_in_min * min_in_hour

let hour_in_day = 24
let ms_in_day = ms_in_hour * hour_in_day

let day_in_week = 7
let ms_in_week = ms_in_day * day_in_week

let day_in_year = 365.24219879
let month_in_year = 12
let day_in_month = day_in_year /. float_of_int(month_in_year)

let ms_in_month = int_of_float ((float_of_int ms_in_day) *. day_in_month)
let ms_in_year =  int_of_float ((float_of_int ms_in_day) *. day_in_year)

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
  let fwd = dv >= 0 in
  let update = if fwd then sub else add in
  let v = abs(dv) / units in
  let new_d = update d (v * units) in
  new_d, v

let static_printer s : int -> int -> string = (fun _ _ -> s)

let empty_printer = static_printer ""

let show_value w =
  (fun _d v ->
    let str = string_of_int v in
    let i = w - String.length str in
    if i > 0
    then (String.make i '0' ^ str)
    else str
  )

let check_condition cmp int true' false' =
  let false' = match false' with
    | None -> empty_printer
    | Some y -> y in
  let cmp = match cmp with
    | `EQ -> (=)
    | `NEQ -> (<>)
    | `LT -> (<)
    | `GT -> (>) in
  (fun d v ->
    if cmp v int
    then true' d v
    else false' d v)

let check_condition_simple cmp true' =
  let cmp = match cmp with
    | `EQ i -> (=) i
    | `LT -> (>) 0
    | `GT -> (<) 0 in
  (fun d v ->
    if cmp d
    then true' d v
    else "")

let directive_block dir expr =
  (fun d _v ->
    let nd,nv = apply_directive dir d in
    expr nd nv)

