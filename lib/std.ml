(** [f1 >> f2] is same as [fun x -> f x |> g]. *)
let[@inline] ( >> ) f g x = g (f x)

let sprintf = Printf.sprintf

module R = struct
  include Result

  type ('a, 'e) t = ('a, 'e) Result.t = Ok of 'a | Error of 'e
  [@@deriving sexp_of]

  module O = struct
    let ( let* ) = bind

    let ( >>= ) = ( let* )

    let ( let+ ) x f = map f x

    let ( >>| ) = ( let+ )
  end
end
