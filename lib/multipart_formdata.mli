(* open Sexplib *)

type file

type t = [ `File of file list | `String of string list ]

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

val parse : header:string -> body:string -> (unit, [> Parser.error ]) result

(* val sexp_of_header : header -> Sexplib.Sexp.t *)
