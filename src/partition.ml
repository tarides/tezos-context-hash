type hash = string
type entry_kind = Node | Content
type entry = { name : string; kind : entry_kind; hash : hash }
type inode_pointer = { index : int; hash : hash }
type 'a encoding = { encoding : string; v : 'a }

type tree = {
  depth : int;
  entries_length : int;
  pointers : (inode_pointer encoding * vs encoding) list;
}

and vs = Values of entry encoding list | Tree of tree

module Util = struct
  module Blake2b = Digestif.Make_BLAKE2B (struct
    let digest_size = 32
  end)

  let blake2b s = Blake2b.digest_string s |> Blake2b.to_raw_string

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
end

let ( ++ ) = ( ^ )
let ( ++. ) = Printf.sprintf "%s%c"
let ( +.+ ) = Printf.sprintf "%c%s"
let ( =++ ) h (prehash, l) = List.fold_left (fun acc x -> acc ++ prehash x) h l
let char_of_kind = function Content -> '\001' | Node -> '\000'

let to_enc_entry ({ name; kind; hash } as entry) =
  let encoding =
    Util.leb128_int (String.length name) ++ name ++. char_of_kind kind ++ hash
  in
  { encoding; v = entry }

let to_enc_pointer ({ index; hash } as pointer) =
  let encoding = Util.leb128_int index ++ hash in
  { encoding; v = pointer }

let to_enc_vs vs =
  let encoding =
    match vs with
    | Values l ->
        '\000' +.+ Util.leb128_int (List.length l) =++ ((fun e -> e.encoding), l)
    | Tree { depth; entries_length; pointers } ->
        '\001'
        +.+ Util.leb128_int depth
        ++ Util.leb128_int entries_length
        ++ Util.leb128_int (List.length pointers)
        =++ ((fun (p, _) -> p.encoding), pointers)
  in
  { encoding; v = vs }

let hash enc_vs = Util.blake2b enc_vs.encoding
let index depth n = Util.ocaml_hash depth n mod Conf.entries

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
