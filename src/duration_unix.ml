module M = Duration.Make
    (struct
      type t = int
      let add = (+)
      let sub = (-)
      let zero = 0
      let to_ms x = x
      let of_ms x = x
      let compare = compare
      let div duration diviseur =
        let d = duration / diviseur in
        if d = 0
        then 0,duration
        else d,duration mod diviseur
    end)(struct
    let sleep x =
      let s = float_of_int x /. 1000. in
      Lwt_unix.sleep s
  end)
include M


