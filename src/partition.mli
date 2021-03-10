open Encoding

type entry = { name : string; kind : char; hash : Hash.t }
type enc_entry
type pointer
type enc_pointer

type vs =
  | Empty
  | Values of enc_entry list
  | Tree of int * int * (enc_pointer * enc_vs) list

and enc_vs = { vsencoding : string; vs : vs }

val partition : entry list -> enc_vs
