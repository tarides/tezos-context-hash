open Encoding

type entry = { name : string; kind : [ `Node | `Contents ]; hash : Hash.t }
type enc_entry = { eencoding : string; entry : entry }
type pointer = { index : int; hash : Hash.t }
type enc_pointer = { pencoding : string; pointer : pointer }

type vs =
  | Empty
  | Values of enc_entry list
  | Tree of {
      depth : int;
      length : int;
      pointers : (enc_pointer * enc_vs) list;
    }

and enc_vs = { vsencoding : string; vs : vs }

val partition : entry list -> enc_vs
