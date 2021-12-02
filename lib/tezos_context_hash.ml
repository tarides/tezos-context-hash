(** {1 Specification implementation}

    This module implements the specification present in SPEC.md, independently
    of the Irmin implementaiton. This code is not intended to be efficient, but
    rather tries to make the correspondance with the specification as clear as
    possible. For this reason, this code should not be used in production. *)

let ( ++ ) = ( ^ )
let list prehash = List.fold_left (fun acc x -> acc ++ prehash x) ""

(** {2 Blake2b} *)

type hash = string

module Blake2b = Digestif.Make_BLAKE2B (struct
  let digest_size = 32
end)

let blake2b s = Blake2b.digest_string s |> Blake2b.to_raw_string

(** {2 Integers}

    See section `Integers` of SPEC.md *)

let fixed_int64 i =
  let buf = Buffer.create 8 in
  Buffer.add_int64_be buf i;
  Buffer.contents buf

let fixed_int i = Int64.of_int i |> fixed_int64

let leb128_int i =
  (* The final size of the result may be smaller (but not greater) than 8. *)
  let buf = Buffer.create 8 in
  let rec loop i =
    let b = i land 127 in
    let i = i lsr 7 in
    (if i <> 0 then b lor 128 else b) |> Buffer.add_uint8 buf;
    if i = 0 then Buffer.contents buf else loop i
  in
  loop i

(** {2 Contents}

    See section `Contents` of SPEC.md *)

let content c = fixed_int (Bytes.length c) ++ Bytes.unsafe_to_string c

(** {2 Commits}

    See the section `Commits` of SPEC.md *)

type commit_metadata = { date : int64; author : string; message : string }

let commit_metadata { date; author; message } =
  fixed_int64 date
  ++ fixed_int (String.length author)
  ++ author
  ++ fixed_int (String.length message)
  ++ message

type commit = { tree : hash; parents : hash list; metadata : commit_metadata }

let commit { tree; parents; metadata } =
  fixed_int 32
  ++ tree
  ++ fixed_int (List.length parents)
  ++ list (fun h -> fixed_int 32 ++ h) (List.sort String.compare parents)
  ++ commit_metadata metadata

(** {2 Trees}

    See section `Trees` of SPEC.md *)

type entry_kind = Content | Node
type tree_entry = { name : string; kind : entry_kind; hash : hash }

(** {3 Nodes}

    When a tree contains at most 256 entries, it is encoded as a flat list of
    entries

    See section `Nodes` of SPEC.md *)

type node_tree = tree_entry list

let node_entry { name; kind; hash } =
  (match kind with
  | Content -> "\255\000\000\000\000\000\000\000"
  | Node -> "\000\000\000\000\000\000\000\000")
  ++ leb128_int (String.length name)
  ++ name
  ++ fixed_int 32
  ++ hash

let node t =
  assert (List.length t <= 256);
  fixed_int (List.length t)
  ++ list node_entry (List.sort (fun e e' -> String.compare e.name e'.name) t)

(** {3 Inodes}

    Trees that contain more than 256 entries are first transforned into inodes,
    before their final encoding.

    See section `Inodes values` of SPEC.md *)

let inode_entry { name; kind; hash } =
  leb128_int (String.length name)
  ++ name
  ++ (match kind with Content -> "\001" | Node -> "\000")
  ++ hash

type inode_value = tree_entry list

let inode_value v =
  assert (List.length v <= 32);
  "\000"
  ++ leb128_int (List.length v)
  ++ list inode_entry (List.sort (fun e e' -> String.compare e.name e'.name) v)

type inode_pointer = { index : int; hash : hash }

let inode_pointer { index; hash } =
  assert (index <= 32);
  leb128_int index ++ hash

type inode_tree = {
  depth : int;
  entries_length : int;
  pointers : inode_pointer list;
}

let inode_tree { depth; entries_length; pointers } =
  assert (List.length pointers <= 32);
  "\001"
  ++ leb128_int depth
  ++ leb128_int entries_length
  ++ leb128_int (List.length pointers)
  ++ list inode_pointer
       (List.sort (fun p p' -> Int.compare p.index p'.index) pointers)

(** {4 Internal tree construction} *)

let ( ** ) = Int32.mul
let ( ^= ) a b = a := Int32.logxor !a b
let ( *= ) a b = a := !a ** b
let ( << ) = Int32.shift_left
let ( >> ) = Int32.shift_right_logical
let ( ||| ) = Int32.logor
let ( &&& ) = Int32.logand

let mix h w =
  w *= 0xcc9e2d51l;
  w := !w << 15 ||| (!w >> 17);
  w *= 0x1b873593l;
  h ^= !w;
  h := !h << 13 ||| (!h >> 19);
  h := Int32.add (!h ** 5l) 0xe6546b64l

let last s off =
  Bytes.init 4 (fun i ->
      let j = off + i in
      if j >= String.length s then '\000' else s.[j])

let ocaml_hash seed s =
  let h = ref (Int32.of_int seed) in
  let w = ref 0l in
  let rec loop i =
    if i + 4 > String.length s then i
    else (
      w := Bytes.get_int32_le (Bytes.unsafe_of_string s) i;
      mix h w;
      loop (i + 4))
  in
  let i = loop 0 in
  if i <> String.length s then (
    w := Bytes.get_int32_le (last s i) 0;
    mix h w);
  h ^= Int32.of_int (String.length s);
  h ^= (!h >> 16);
  h *= 0x85ebca6bl;
  h ^= (!h >> 13);
  h *= 0xc2b2ae35l;
  h ^= (!h >> 16);
  Int32.to_int (!h &&& 0x3FFFFFFFl)

let index d n = ocaml_hash d n mod 32

type inode = Empty | Value of inode_value | Tree of inode_tree

(** This implements the computation of $X_\{d,j\}$. *)
let filter x d j = List.filter (fun { name; _ } -> index d name = j) x

let rec partition d x =
  match List.length x with
  | 0 -> Empty
  | n when n <= 32 -> Value x
  | n ->
      let tp =
        List.init 32 (fun j ->
            let tj = partition (succ d) (filter x d j) in
            let tj_prehash =
              match tj with
              | Empty -> inode_value []
              | Value v -> inode_value v
              | Tree t -> inode_tree t
            in
            (tj, { index = j; hash = blake2b tj_prehash }))
      in
      let p =
        List.filter_map
          (fun (ti, pi) -> match ti with Empty -> None | _ -> Some pi)
          tp
      in
      Tree { depth = d; entries_length = n; pointers = p }

let inode x =
  match partition 0 x with
  | Value v -> inode_value v
  | Tree t -> inode_tree t
  | Empty -> (* This should not happen *) inode_value []

let tree x = if List.length x > 256 then inode x else node x
