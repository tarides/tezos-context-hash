open Irmin_tezos
open Encoding
module Node = Irmin.Private.Node.Make (Hash) (Path) (Metadata)
module Inter = Irmin_pack.Private.Inode.Make_intermediate (Conf) (Hash) (Node)
module Index = Irmin_pack.Index.Make (Hash)
module H_contents = Irmin.Hash.Typed (Hash) (Contents)

module Spec = struct
  type kind = Tree | Contents [@@deriving irmin]

  type entry = { name : Node.step; kind : kind; hash : Node.hash }
  [@@deriving irmin]

  type inode = Inter.Val.Concrete.t [@@deriving irmin]

  type node = { hash : Node.hash; bindings : entry list; inode : inode option }
  [@@deriving irmin]

  let entry (s, b) =
    match b with
    | `Node h -> { name = s; kind = Tree; hash = h }
    | `Contents (h, _) -> { name = s; kind = Contents; hash = h }

  let inode = Inter.Val.to_concrete

  let of_t (t : Inter.Val.t) =
    let inode =
      if Inter.Val.length t <= Conf.stable_hash then None else Some (inode t)
    in
    let bt = Inter.Val.to_bin t in
    Irmin.Type.to_json_string node_t
      {
        hash = Inter.Elt.hash bt;
        bindings = List.map entry (Inter.Val.list t);
        inode;
      }
end

let contents x = `Contents (x, Metadata.default)
let node x = `Node x

module Gen = struct
  let init = Random.init
  let char () = char_of_int (33 + Random.int 94)
  let int () = Random.int ((1 lsl 30) - 1)
  let fixed_string n () = String.init n (fun _ -> char ())
  let string () = fixed_string (1 + Random.int (1 lsl 5)) ()
  let fixed_bytes n () = Bytes.init n (fun _ -> char ())
  let bytes () = fixed_bytes (Random.int (1 lsl 10)) ()
  let fixed_list n gen () = List.init n (fun _ -> gen ())
  let pair gen1 gen2 () = (gen1 (), gen2 ())
  let content () = bytes ()
  let hash () = content () |> H_contents.hash
  let atom () = hash () |> if Random.bool () then contents else node
  let fixed_inode n () = fixed_list n (pair string atom) () |> Inter.Val.v

  let long_inode () =
    let len = Conf.stable_hash + Random.int (1 lsl 10) in
    fixed_inode len ()

  let short_inode () =
    let len = 1 + Random.int Conf.stable_hash in
    fixed_inode len ()
end

let generate_ocaml_hash_cases n dir seed =
  let path = Filename.concat dir "ocaml_hash.json" in
  Fmt.pr "Generating ocaml_hash test cases in `%s'@." path;
  let oc = open_out path in
  Gen.init seed;
  Gen.(fixed_list n (pair string int)) ()
  |> List.map (fun (s, seed) ->
         `Assoc
           [
             ("s", `String s);
             ("seed", `Int seed);
             ("ocaml_hash", `Int (Hashtbl.seeded_hash seed s));
           ])
  |> (fun l -> `List l)
  |> Yojson.to_channel oc;
  close_out oc

let to_json oc inodes =
  let open Fmt in
  kstr (output_string oc) "%a" (list ~sep:nop (using Spec.of_t string)) inodes

let generate_inode_cases msg gen n dir seed =
  let path = Fmt.kstr (Filename.concat dir) "inodes_%s.json" msg in
  Fmt.pr "Generating %s inode test cases in `%s'@." msg path;
  let oc = open_out path in
  Gen.init seed;
  Gen.fixed_list n gen () |> to_json oc;
  close_out oc

let generate n dir seed =
  Printexc.record_backtrace true;
  generate_ocaml_hash_cases n dir seed;
  generate_inode_cases "short" Gen.short_inode n dir seed;
  generate_inode_cases "long" Gen.long_inode n dir seed

open Cmdliner

let rec mkdir s =
  (match Filename.dirname s with "." -> () | parent -> mkdir parent);
  Unix.mkdir (Filename.basename s) 0o755

let directory =
  let parse s =
    match Sys.file_exists s with
    | true ->
        if Sys.is_directory s then `Ok s
        else `Error (Fmt.str "`%s' is not a directory" s)
    | false ->
        mkdir s;
        `Ok s
  in
  (parse, Fmt.string)

let seed =
  let doc = "Seed used to generate random data." in
  Arg.(value & opt int 0 & info [ "s"; "seed" ] ~doc)

let number =
  let doc = "Number of data points to generate for each test." in
  Arg.(value & opt int 100 & info [ "n"; "number" ] ~doc)

let dir =
  let doc = "The directory where the files will be written." in
  Arg.(value & opt directory "data" & info [ "d"; "directory" ] ~doc)

let cmd =
  let doc = "Irmin hash test cases generation" in
  Term.(const generate $ number $ dir $ seed, info "irmin-hash-gen" ~doc)

let () = Term.(exit @@ eval cmd)
