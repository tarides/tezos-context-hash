open Encoding

type entry_kind = Node | Content
type entry = { name : string; kind : entry_kind; hash : Hash.t }
type inode_pointer = { index : int; hash : Hash.t }
type 'a encoding = { encoding : string; v : 'a }

type tree = {
  depth : int;
  entries_length : int;
  pointers : (inode_pointer encoding * vs encoding) list;
}

and vs = Empty | Values of entry encoding list | Tree of tree

val partition : entry list -> vs encoding
val enc_vs_t : vs encoding Irmin.Type.t
