open Monolith

module type Testable = sig
  type hash = string
  type commit_metadata = { date : int64; author : string; message : string }
  type commit = { tree : hash; parents : hash list; metadata : commit_metadata }
  type entry_kind = Content | Node
  type tree_entry = { name : string; kind : entry_kind; hash : hash }

  val fixed_int : int -> string
  val leb128_int : int -> string
  val content : bytes -> string
  val commit : commit -> string
  val ocaml_hash : int -> string -> int
  val tree : tree_entry list -> string
end

module C : Testable = Spec

module R :
  Testable
    with type hash = C.hash
     and type commit_metadata = C.commit_metadata
     and type commit = C.commit
     and type entry_kind = C.entry_kind
     and type tree_entry = C.tree_entry = struct
  type hash = string

  let with_encoder encoder x =
    let buf = Buffer.create 0 in
    encoder x (Buffer.add_string buf);
    Buffer.contents buf

  let to_irmin_hash =
    let encode =
      Irmin.Type.(unstage (of_bin_string Irmin_tezos.Encoding.Hash.t))
    in
    fun x -> encode x |> Result.get_ok

  let fixed_int =
    let encode = Irmin.Type.(unstage (encode_bin int64)) in
    fun i -> with_encoder encode (Int64.of_int i)

  let leb128_int =
    let encode = Irmin.Type.(unstage (encode_bin int)) in
    with_encoder encode

  let content =
    let encode =
      Irmin.Type.(unstage (pre_hash Irmin_tezos.Encoding.Contents.t))
    in
    with_encoder encode

  type commit_metadata = C.commit_metadata = {
    date : int64;
    author : string;
    message : string;
  }

  type commit = C.commit = {
    tree : hash;
    parents : hash list;
    metadata : commit_metadata;
  }

  let commit =
    let encode =
      Irmin.Type.(unstage (pre_hash Irmin_tezos.Encoding.Commit.t))
    in
    let to_irmin_metadata { date; author; message } =
      Irmin.Info.v ~date ~author message
    in
    let to_irmin_commit { tree; parents; metadata } =
      let info = to_irmin_metadata metadata in
      Irmin_tezos.Encoding.Commit.v ~info ~node:(to_irmin_hash tree)
        ~parents:(List.map to_irmin_hash parents)
    in
    fun c -> with_encoder encode (to_irmin_commit c)

  type entry_kind = C.entry_kind = Content | Node

  type tree_entry = C.tree_entry = {
    name : string;
    kind : entry_kind;
    hash : hash;
  }

  let ocaml_hash = Hashtbl.seeded_hash

  module Inter =
    Irmin_pack.Private.Inode.Make_intermediate
      (Irmin_tezos.Conf)
      (Irmin_tezos.Encoding.Hash)
      (Irmin_tezos.Encoding.Node)

  let tree =
    let encode = Irmin.Type.(unstage (pre_hash Inter.Val.t)) in
    let to_irmin_entry { name; kind; hash } =
      let hash = to_irmin_hash hash in
      ( name,
        match kind with
        | Content -> `Contents (hash, Irmin_tezos.Encoding.Metadata.default)
        | Node -> `Node hash )
    in
    let to_irmin_tree l = List.map to_irmin_entry l |> Inter.Val.v in
    fun t -> with_encoder encode (to_irmin_tree t)
end

(** Custom generators absent from Monolith *)
module G = struct
  let hash () = Gen.string (fun () -> 32) Gen.char ()
  let string () = Gen.string (Gen.closed_interval 1 64) Gen.char ()
  let bytes () = Bytes.of_string (string ())
  let int64 () = Int64.of_int (Gen.int Int.max_int ())

  let commit_metadata () =
    C.{ date = int64 (); author = string (); message = string () }

  let commit () =
    {
      C.tree = hash ();
      C.parents = Gen.list (Gen.closed_interval 1 32) hash ();
      C.metadata = commit_metadata ();
    }

  let tree_entry () =
    {
      C.name = string ();
      C.kind = Gen.choose [ C.Content; C.Node ] ();
      C.hash = hash ();
    }
end

(** Custom printers absent from PPrint *)
module P = struct
  open PPrintOCaml

  let commit_metadata C.{ date; author; message } =
    record ""
      [
        ("date", int64 date);
        ("author", string author);
        ("message", string message);
      ]

  let commit C.{ tree; parents; metadata } =
    record ""
      [
        ("tree", string tree);
        ("parents", (list string) parents);
        ("metadata", commit_metadata metadata);
      ]

  let tree_entry C.{ name; kind; hash } =
    record ""
      [
        ("name", string name);
        ( "kind",
          string (match kind with Node -> "Node" | Content -> "Content") );
        ("hash", string hash);
      ]
end

let positive_int = int_within (Gen.int Int.max_int)

let bytes =
  easily_constructible G.bytes (fun b ->
      PPrintOCaml.string (Bytes.unsafe_to_string b))

let string =
  let neg = easily_constructible G.string PPrintOCaml.string in
  let pos = deconstructible PPrintOCaml.string in
  ifpol neg pos

let int64 =
  easily_constructible G.int64 (fun i -> PPrint.string (Int64.to_string i))

let commit_metadata = easily_constructible G.commit_metadata P.commit_metadata
let commit = easily_constructible G.commit P.commit
let tree_entry = easily_constructible G.tree_entry P.tree_entry

let distinct_names =
  let tbl = Hashtbl.create 0 in
  fun l ->
    Hashtbl.clear tbl;
    let rec loop = function
      | [] -> true
      | h :: t ->
          let name = h.C.name in
          if Hashtbl.mem tbl name then false
          else (
            Hashtbl.add tbl name ();
            loop t)
    in
    loop l

let short_entry_list =
  distinct_names % list ~length:(Gen.closed_interval 1 256) tree_entry

let long_entry_list =
  distinct_names % list ~length:(Gen.closed_interval 257 1024) tree_entry

let () =
  let spec = positive_int ^> string in
  declare "fixed int" spec R.fixed_int C.fixed_int;

  let spec = positive_int ^> string in
  declare "LEB128 int" spec R.leb128_int C.leb128_int;

  let spec = bytes ^> string in
  declare "content" spec R.content C.content;

  let spec = commit ^> string in
  declare "commit" spec R.commit C.commit;

  let spec = positive_int ^> string ^> int in
  declare "ocaml_hash" spec R.ocaml_hash C.ocaml_hash;

  let spec = short_entry_list ^> string in
  declare "node" spec R.tree C.tree;

  let spec = long_entry_list ^> string in
  declare "inode" spec R.tree C.tree

let () =
  let fuel = 10 in
  main fuel
