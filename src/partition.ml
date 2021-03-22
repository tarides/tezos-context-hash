open Encoding
module H_contents = Irmin.Hash.Typed (Hash) (Contents)

type entry_kind = Node | Content [@@deriving irmin]

type entry = { name : string; kind : entry_kind; hash : Hash.t }
[@@deriving irmin]

type inode_pointer = { index : int; hash : Hash.t } [@@deriving irmin]
type 'a encoding = { encoding : string; v : 'a } [@@deriving irmin]

type tree = {
  depth : int;
  entries_length : int;
  pointers : (inode_pointer encoding * vs encoding) list;
}

and vs = Empty | Values of entry encoding list | Tree of tree

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
  variant "t" (fun empty values tree -> function
    | Empty -> empty | Values l -> values l | Tree t -> tree t)
  |~ case0 "Empty" Empty
  |~ case1 "Values" (list (encoding_t entry_t)) (fun l -> Values l)
  |~ case1 "Tree" tree_t (fun t -> Tree t)
  |> sealv

let _, vs_t = Irmin.Type.mu2 (fun tree_t vs_t -> (mktree vs_t, mkvs_t tree_t))
let enc_vs_t = encoding_t vs_t

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
    if i = 0 then Buffer.contents buf
    else
      let b = i land 127 in
      let i = i lsr 7 in
      (if i <> 0 then b lor 128 else b) |> Buffer.add_uint8 buf;
      loop i
  in
  loop i

let compute_hash s = H_contents.hash (Bytes.of_string s)
let hasht_to_string h = Irmin.Type.(unstage (to_bin_string Encoding.Hash.t) h)
let char_of_kind = function Content -> '\001' | Node -> '\000'

let to_enc_entry ({ name; kind; hash } as entry) =
  let encoding =
    Format.asprintf "%s%s%c%s"
      (leb128_int @@ String.length name)
      name (char_of_kind kind) (hasht_to_string hash)
  in
  (* Format.eprintf "Enc entry: %S(%s):@.  %S (%d)@." len name eencoding
   *   (String.length eencoding); *)
  { encoding; v = entry }

let to_enc_pointer ({ index; hash } as pointer) =
  let encoding =
    Format.asprintf "%s%s" (leb128_int index) (hasht_to_string hash)
  in
  (* Format.eprintf "Enc pointer: %S@.  %S (%d)@." index pencoding
   *   (String.length pencoding); *)
  { encoding; v = pointer }

let pp_sep fmt () = Format.fprintf fmt ""

let to_enc_vs vs =
  let encoding =
    match vs with
    | Values l ->
        Format.asprintf "\000%s%a"
          (leb128_int (List.length l))
          (Format.pp_print_list ~pp_sep (fun fmt e ->
               Format.fprintf fmt "%s" e.encoding))
          l
    | Tree { depth; entries_length; pointers } ->
        Format.asprintf "\001%s%s\032%a" (leb128_int depth)
          (leb128_int entries_length)
          (Format.pp_print_list ~pp_sep (fun fmt (p, _) ->
               Format.fprintf fmt "%s" p.encoding))
          pointers
    | Empty -> Format.asprintf "\000\n"
  in
  { encoding; v = vs }

let hash enc_vs = compute_hash enc_vs.encoding

let ocaml_hash seed s =
  (* TODO *)
  Hashtbl.seeded_hash seed s

let index depth n = ocaml_hash depth n mod 32

let subset ~depth i l =
  List.filter (fun { v = { name; _ }; _ } -> index depth name = i) l

let partition l =
  let l = List.map to_enc_entry l in
  let rec aux depth l =
    to_enc_vs
    @@
    match List.length l with
    | 0 -> Empty
    | n when n <= Conf.entries ->
        Values (List.sort (fun e1 e2 -> Stdlib.compare e1.v.name e2.v.name) l)
    | _ ->
        let pointers =
          List.init Conf.entries (fun i ->
              (i, aux (depth + 1) (subset ~depth i l)))
          |> List.filter (fun (_, { v; _ }) ->
                 match v with Empty -> false | _ -> true)
          |> List.map (fun (i, vs) ->
                 let hash = hash vs in
                 (* Format.eprintf "pp_hash: %a@." pp_hash hash; *)
                 let hp = to_enc_pointer { index = i; hash } in
                 (hp, vs))
        in
        Tree { depth; entries_length = List.length l; pointers }
  in
  aux 0 l
