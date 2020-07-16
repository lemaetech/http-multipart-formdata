(* open Sexplib *)

type file

type t = [ `File of file list | `String of string list ]

type error =
  [ `Boundary_parameter_not_found
  | `Not_multipart_formdata_header
  | Parser.error ]

(* module Content_type : sig *)
(*   type t *)

(*   val sexp_of_t : t -> Sexplib.Sexp.t *)

(*   val pp : Format.formatter -> t -> unit *)

(*   val ty : t -> string *)

(*   val subtype : t -> string *)

(*   val parameters : t -> (string * string) Seq.t *)

(*   val find_parameter : name:string -> t -> string option *)
(* end *)

(* type header *)

val parse : header:string -> body:Parser.src -> (string list, [> error ]) result

(* val sexp_of_header : header -> Sexplib.Sexp.t *)
