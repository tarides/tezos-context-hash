open Encoding
module H_contents = Irmin.Hash.Typed (Hash) (Contents)

type entry = { name : string; kind : char; hash : Hash.t }
type enc_entry = { eencoding : string; entry : entry }
type pointer = { index : int; hash : Hash.t }
type enc_pointer = { pencoding : string; pointer : pointer }

type vs =
  | Empty
  | Values of enc_entry list
  | Tree of int * int * (enc_pointer * enc_vs) list

and enc_vs = { vsencoding : string; vs : vs }

let pre_hash t v =
  let buf = Buffer.create 13 in
  let pre_hash = Irmin.Type.(unstage (pre_hash t)) in
  pre_hash v (Buffer.add_string buf);
  Buffer.contents buf

let compute_hash s = H_contents.hash (Bytes.of_string s)
let pp_hash fmt h = Irmin.Type.(pp Encoding.Hash.t) fmt h

let to_enc_entry ({ name; kind; hash } as entry) =
  let len = pre_hash Irmin.Type.int (String.length name) in
  let eencoding = Format.asprintf "%s%s%c%a" len name kind pp_hash hash in
  { eencoding; entry }

let to_enc_pointer ({ index; hash } as pointer) =
  let pencoding = Format.asprintf "%i%a" index pp_hash hash in
  { pencoding; pointer }

let pp_sep fmt () = Format.fprintf fmt ""
let pp_int_to_leb128 fmt i = Format.fprintf fmt "%s" (pre_hash Irmin.Type.int i)

let to_enc_vs vs =
  let vsencoding =
    match vs with
    | Values l ->
        Format.asprintf "\000\n%a"
          (Format.pp_print_list ~pp_sep (fun fmt e ->
               Format.fprintf fmt "%s" e.eencoding))
          l
    | Tree (depth, len, l) ->
        Format.asprintf "\001%a%a\032%a" pp_int_to_leb128 depth pp_int_to_leb128
          len
          (Format.pp_print_list ~pp_sep (fun fmt (p, _) ->
               Format.fprintf fmt "%s" p.pencoding))
          l
    | Empty -> Format.asprintf "\000\n"
  in
  { vsencoding; vs }

let hash enc_vs = compute_hash enc_vs.vsencoding
let hash_key = Irmin.Type.(unstage (short_hash Node.step_t))
let index ~depth k = abs (hash_key ~seed:depth k) mod Conf.entries

let subset ~depth i l =
  List.filter (fun { entry = { name; _ }; _ } -> index ~depth name = i) l

let partition l =
  let l = List.map to_enc_entry l in
  let rec aux depth l =
    to_enc_vs
    @@
    match List.length l with
    | 0 -> Empty
    | n when n <= Conf.entries ->
        Values
          (List.sort
             (fun e1 e2 -> Stdlib.compare e1.entry.name e2.entry.name)
             l)
    | _ ->
        let children =
          List.init Conf.entries (fun i ->
              (i, aux (depth + 1) (subset ~depth i l)))
          |> List.filter (fun (_, { vs; _ }) ->
                 match vs with Empty -> false | _ -> true)
          |> List.map (fun (i, vs) ->
                 let hash = hash vs in
                 let hp = to_enc_pointer { index = i; hash } in
                 (hp, vs))
        in
        Tree (depth, List.length l, children)
  in
  aux 0 l
