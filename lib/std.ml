(** [f1 >> f2] is same as [fun x -> f x |> g]. *)
let[@inline] ( >> ) f g x = g (f x)

let sprintf = Printf.sprintf

let asprintf = Format.asprintf

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

  let errorf fmt = Format.kasprintf (fun s -> error s) fmt
end

module String = struct
  include StringLabels

  let starts_with ~prefix s =
    let prefix_len = length prefix in
    let full_len = length s in
    full_len >= prefix_len && String.equal (sub s ~pos:0 ~len:prefix_len) prefix

  let chop_prefix ~prefix s =
    let prefix_len = length prefix in
    let full_len = length s in
    if
      full_len >= prefix_len
      && String.equal (sub s ~pos:0 ~len:prefix_len) prefix
    then sub s ~pos:prefix_len ~len:(full_len - prefix_len)
    else s
end
