open Encoding
module H_contents = Irmin.Hash.Typed (Hash) (Contents)

type entry = { name : string; kind : [ `Node | `Contents ]; hash : Hash.t }
[@@deriving irmin]

type pointer = { index : int; hash : Hash.t } [@@deriving irmin]
type 'a encoding = { encoding : string; v : 'a } [@@deriving irmin]

type tree = {
  depth : int;
  length : int;
  pointers : (pointer encoding * vs encoding) list;
}

and vs = Empty | Values of entry encoding list | Tree of tree

let mktree vs_t =
  let open Irmin.Type in
  record "tree" (fun depth length pointers -> { depth; length; pointers })
  |+ field "depth" int (fun t -> t.depth)
  |+ field "length" int (fun t -> t.length)
  |+ field "pointers"
       (list (pair (encoding_t pointer_t) (encoding_t vs_t)))
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

let pp_int_to_leb128 fmt i =
  let pre_hash v =
    let buf = Buffer.create 13 in
    let pre_hash = Irmin.Type.(unstage (pre_hash Irmin.Type.int)) in
    pre_hash v (Buffer.add_string buf);
    Buffer.contents buf
  in
  Format.fprintf fmt "%s" (pre_hash i)

let compute_hash s = H_contents.hash (Bytes.of_string s)
let to_bin_hash h = Irmin.Type.(unstage (to_bin_string Encoding.Hash.t) h)
let char_of_kind = function `Contents -> '\001' | `Node -> '\000'

let to_enc_entry ({ name; kind; hash } as entry) =
  let encoding =
    Format.asprintf "%a%s%c%s" pp_int_to_leb128 (String.length name) name
      (char_of_kind kind) (to_bin_hash hash)
  in
  (* Format.eprintf "Enc entry: %S(%s):@.  %S (%d)@." len name eencoding
   *   (String.length eencoding); *)
  { encoding; v = entry }

let to_enc_pointer ({ index; hash } as pointer) =
  let encoding =
    Format.asprintf "%a%s" pp_int_to_leb128 index (to_bin_hash hash)
  in
  (* Format.eprintf "Enc pointer: %S@.  %S (%d)@." index pencoding
   *   (String.length pencoding); *)
  { encoding; v = pointer }

let pp_sep fmt () = Format.fprintf fmt ""

let to_enc_vs vs =
  let encoding =
    match vs with
    | Values l ->
        Format.asprintf "\000%a%a" pp_int_to_leb128 (List.length l)
          (Format.pp_print_list ~pp_sep (fun fmt e ->
               Format.fprintf fmt "%s" e.encoding))
          l
    | Tree { depth; length; pointers } ->
        Format.asprintf "\001%a%a\032%a" pp_int_to_leb128 depth pp_int_to_leb128
          length
          (Format.pp_print_list ~pp_sep (fun fmt (p, _) ->
               Format.fprintf fmt "%s" p.encoding))
          pointers
    | Empty -> Format.asprintf "\000\n"
  in
  { encoding; v = vs }

let hash enc_vs = compute_hash enc_vs.encoding
let hash_key = Irmin.Type.(unstage (short_hash Node.step_t))
let index ~depth k = abs (hash_key ~seed:depth k) mod Conf.entries

let subset ~depth i l =
  List.filter (fun { v = { name; _ }; _ } -> index ~depth name = i) l

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
        Tree { depth; length = List.length l; pointers }
  in
  aux 0 l
