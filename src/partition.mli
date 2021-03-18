open Encoding

type entry = { name : string; kind : [ `Node | `Contents ]; hash : Hash.t }
type pointer = { index : int; hash : Hash.t }
type 'a encoding = { encoding : string; v : 'a }

type tree = {
  depth : int;
  length : int;
  pointers : (pointer encoding * vs encoding) list;
}

and vs = Empty | Values of entry encoding list | Tree of tree

val partition : entry list -> vs encoding
val enc_vs_t : vs encoding Irmin.Type.t
