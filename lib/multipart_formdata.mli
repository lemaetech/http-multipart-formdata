open Sexplib

type file

type t = [ `File of file list | `String of string list ]

module Content_type : sig
  type t

  val sexp_of_t : t -> Sexp.t

  val pp : Format.formatter -> t -> unit

  val ty : t -> string

  val subtype : t -> string

  val parameters : t -> (string * string) Seq.t

  val find_parameter : name:string -> t -> string option
end

type header

val sexp_of_header : header -> Sexp.t

open Parser

val parse : string -> (header, [> Parser.error ]) result
