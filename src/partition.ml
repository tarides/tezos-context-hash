type hash = string [@@deriving irmin]
type entry_kind = Node | Content [@@deriving irmin]

type entry = { name : string; kind : entry_kind; hash : hash }
[@@deriving irmin]

type inode_pointer = { index : int; hash : hash } [@@deriving irmin]
type 'a encoding = { encoding : string; v : 'a } [@@deriving irmin]

type tree = {
  depth : int;
  entries_length : int;
  pointers : (inode_pointer encoding * vs encoding) list;
}

and vs = Values of entry encoding list | Tree of tree

let mktree vs_t =
  let open Irmin.Type in
  record "tree" (fun depth entries_length pointers ->
      { depth; entries_length; pointers })
  |+ field "depth" int (fun t -> t.depth)
  |+ field "length" int (fun t -> t.entries_length)
  |+ field "pointers"
       (list (pair (encoding_t inode_pointer_t) (encoding_t vs_t)))
       (fun t -> t.pointers)
  |> sealr

let mkvs_t tree_t =
  let open Irmin.Type in
  variant "t" (fun values tree -> function
    | Values l -> values l | Tree t -> tree t)
  |~ case1 "Values" (list (encoding_t entry_t)) (fun l -> Values l)
  |~ case1 "Tree" tree_t (fun t -> Tree t)
  |> sealv

let _, vs_t = Irmin.Type.mu2 (fun tree_t vs_t -> (mktree vs_t, mkvs_t tree_t))
let enc_vs_t = encoding_t vs_t

module Util = struct
  module Blake2b = Digestif.Make_BLAKE2B (struct
    let digest_size = 32
  end)

  let blake2b s = Blake2b.digest_string s |> Blake2b.to_raw_string

  (* let fixed_int64 i =
   *   let buf = Buffer.create 8 in
   *   Buffer.add_int64_be buf i;
   *   Buffer.contents buf
   *
   * let fixed_int i = Int64.of_int i |> fixed_int64 *)

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
end

let ( ++ ) = ( ^ )
let ( ++. ) = Printf.sprintf "%s%c"
let ( +.+ ) = Printf.sprintf "%c%s"
let ( =++ ) h (prehash, l) = List.fold_left (fun acc x -> acc ++ prehash x) h l
let char_of_kind = function Content -> '\001' | Node -> '\000'

let to_enc_entry ({ name; kind; hash } as entry) =
  let encoding =
    Util.leb128_int (String.length name)
    ++ name
    ++. char_of_kind kind
    ++ Util.blake2b hash
  in
  (* Format.eprintf "Enc entry: %S(%s):@.  %S (%d)@.  %S@."
   *   (leb128_int @@ String.length name)
   *   name encoding (String.length encoding)
   *   (Irmin.Type.(unstage (to_bin_string entry_t)) entry); *)
  { encoding; v = entry }

let to_enc_pointer ({ index; hash } as pointer) =
  let encoding = Util.leb128_int index ++ hash in
  (* Format.eprintf "Enc pointer: @.  %S (%d)@.  %S@." encoding
   *   (String.length encoding)
   *   (Irmin.Type.(unstage (to_bin_string inode_pointer_t)) pointer); *)
  { encoding; v = pointer }

let to_enc_vs vs =
  let encoding =
    match vs with
    | Values l ->
        (* Format.eprintf "Debug V:@.%d@[<v 0>@,%a@]@." (List.length l)
         *   (Format.pp_print_list (fun fmt e ->
         *        Format.fprintf fmt "%S" e.encoding))
         *   l;
         * Format.eprintf "128 : %S@." (Util.leb128_int (List.length l)); *)
        '\000' +.+ Util.leb128_int (List.length l) =++ ((fun e -> e.encoding), l)
    | Tree { depth; entries_length; pointers } ->
        (* Format.eprintf "Debug T:@.%d@[<v 0>@,%a@]@." (List.length pointers)
         *   (Format.pp_print_list (fun fmt (p, _) ->
         *        Format.fprintf fmt "%S" p.encoding))
         *   pointers; *)
        '\001'
        +.+ Util.leb128_int depth
        ++ Util.leb128_int entries_length
        ++ Util.leb128_int (List.length pointers)
        =++ ((fun (p, _) -> p.encoding), pointers)
  in
  (* Format.eprintf "Enc value: @.  %S (%d)@.  %S@." encoding
   *   (String.length encoding)
   *   (Irmin.Type.(unstage (to_bin_string vs_t)) vs); *)
  { encoding; v = vs }

let hash enc_vs = Util.blake2b enc_vs.encoding

let ocaml_hash seed s =
  (* TODO *)
  Hashtbl.seeded_hash seed s

let index depth n = ocaml_hash depth n mod Conf.entries

let subset ~depth i l =
  List.filter (fun { v = { name; _ }; _ } -> index depth name = i) l

let partition l =
  let l = List.map to_enc_entry l in
  let rec aux depth l =
    to_enc_vs
    @@
    match List.length l with
    | 0 -> Values []
    | n when n <= Conf.entries ->
        Values (List.sort (fun e1 e2 -> Stdlib.compare e1.v.name e2.v.name) l)
    | _ ->
        let pointers =
          List.init Conf.entries (fun i ->
              (i, aux (depth + 1) (subset ~depth i l)))
        in
        let pointers =
          pointers
          |> List.filter (fun (_, { v; _ }) ->
                 match v with Values [] -> false | _ -> true)
          |> List.map (fun (i, vs) ->
                 let hash = hash vs in
                 (* Format.eprintf "pp_hash: %a@." pp_hash hash; *)
                 let hp = to_enc_pointer { index = i; hash } in
                 (hp, vs))
        in
        Tree { depth; entries_length = List.length l; pointers }
  in
  aux 0 l
