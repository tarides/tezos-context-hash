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

module R : Testable = struct
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

  type commit_metadata = { date : int64; author : string; message : string }
  type commit = { tree : hash; parents : hash list; metadata : commit_metadata }

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

  type entry_kind = Content | Node
  type tree_entry = { name : string; kind : entry_kind; hash : hash }

  let ocaml_hash = Hashtbl.seeded_hash

  let tree =
    let encode = Irmin.Type.(unstage (pre_hash Irmin_tezos.Encoding.Node.t)) in
    let to_irmin_entry { name; kind; hash } =
      let hash = to_irmin_hash hash in
      ( name,
        match kind with
        | Content -> `Contents (hash, Irmin_tezos.Encoding.Metadata.default)
        | Node -> `Node hash )
    in
    let to_irmin_tree l =
      List.map to_irmin_entry l |> Irmin_tezos.Encoding.Node.v
    in
    fun t -> with_encoder encode (to_irmin_tree t)
end

let positive_int = int_within (Gen.int Int.max_int)

let bytes =
  easily_constructible
    (fun () ->
      Bytes.unsafe_of_string (Gen.string (Gen.int (1 lsl 5)) Gen.char ()))
    (fun b -> PPrintOCaml.string (Bytes.to_string b))

let string =
  let neg =
    easily_constructible
      (Gen.string (Gen.int (1 lsl 5)) Gen.char)
      PPrintOCaml.string
  in
  let pos = deconstructible PPrint.string in
  ifpol neg pos

let () =
  let spec = positive_int ^> string in
  declare "fixed int" spec R.fixed_int C.fixed_int;

  let spec = positive_int ^> string in
  declare "LEB128 int" spec R.leb128_int C.leb128_int;

  let spec = bytes ^> string in
  declare "contents" spec R.content C.content;

  let spec = string ^> int in
  declare "ocaml_hash" spec (R.ocaml_hash 0) (C.ocaml_hash 0)

let () =
  let fuel = 10 in
  main fuel
