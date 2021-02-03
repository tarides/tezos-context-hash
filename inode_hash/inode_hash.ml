open Irmin_tezos
module Node = Irmin.Private.Node.Make (Hash) (Path) (Metadata)
module Inter = Irmin_pack.Private.Inode.Make_intermediate (Conf) (Hash) (Node)
module Index = Irmin_pack.Index.Make (Hash)
module H_contents = Irmin.Hash.Typed (Hash) (Contents)

let contents x = `Contents (x, Metadata.default)
let node x = `Node x

module Gen = struct
  let init = Random.init
  let full_init = Random.full_init
  let char () = char_of_int (Random.int 256)
  let fixed_string n () = String.init n (fun _ -> char ())
  let string () = fixed_string (Random.int (1 lsl 10)) ()
  let fixed_bytes n () = Bytes.init n (fun _ -> char ())
  let bytes () = fixed_bytes (Random.int (1 lsl 10)) ()
  let fixed_list n gen () = List.init n (fun _ -> gen ())
  let list gen () = fixed_list (Random.int (1 lsl 10)) gen ()
  let pair gen1 gen2 () = (gen1 (), gen2 ())
  let content () = bytes ()
  let hash () = content () |> H_contents.hash
  let atom () = hash () |> if Random.bool () then contents else node
  let fixed_inode n () = fixed_list n (pair string atom) () |> Inter.Val.v
  let inode () = list (pair string atom) () |> Inter.Val.v

  let long_inode () =
    let len = Conf.stable_hash + Random.int (1 lsl 10) in
    fixed_inode len ()

  let short_inode () =
    let len = 1 + Random.int Conf.stable_hash in
    fixed_inode len ()
end

let to_json (inode : Inter.Val.t list) : bytes =
  ignore inode;
  failwith "TODO"

let run n short long path =
  if short && long then invalid_arg "short and long options are incompatible";
  let oc = open_out path in
  Gen.fixed_list n
    (if short then Gen.short_inode
    else if long then Gen.long_inode
    else Gen.inode)
    ()
  |> to_json
  |> output_bytes oc;
  close_out oc

open Cmdliner

let inodes_number =
  let doc = "Number of inodes to generate." in
  Arg.(value & opt int 1_000 & info [ "n"; "inodes-number" ] ~doc)

let short_inodes =
  let doc = "Only generate short inodes." in
  Arg.(value & opt bool false & info [ "short" ] ~doc)

let long_inodes =
  let doc = "Only generate long inodes." in
  Arg.(value & opt bool false & info [ "long" ] ~doc)

let path =
  let doc = "The file where the result will be printed." in
  Arg.(value & opt string "inodes.json" & info [ "o"; "output" ] ~doc)

let cmd =
  let doc = "Irmin inodes generation" in
  Term.
    ( const run $ inodes_number $ short_inodes $ long_inodes $ path,
      info "inodes-gen" ~doc )

let () = Term.(exit @@ eval cmd)
