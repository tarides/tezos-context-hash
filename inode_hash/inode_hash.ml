open Irmin_tezos
module Node = Irmin.Private.Node.Make (Hash) (Path) (Metadata)
module Inter = Irmin_pack.Private.Inode.Make_intermediate (Conf) (Hash) (Node)
module Index = Irmin_pack.Index.Make (Hash)
module H_contents = Irmin.Hash.Typed (Hash) (Contents)

let contents x = `Contents (x, Metadata.default)
let node x = `Node x

let progress verbose n =
  if verbose then
    let bar message =
      Progress_unix.counter ~mode:`UTF8 ~total:(Int64.of_int n) ~message
        ~pp:Progress.Units.bytes ()
    in
    Progress_unix.(
      with_reporters (bar "Generating     " / bar "Serialising    "))
    @@ fun (g, s) ->
    let progress b () = match b with `Generating -> g | `Serialising -> s in
    progress
  else fun _ _ _ -> ()

module Gen = struct
  (* let init = Random.init
   * let full_init = Random.full_init *)
  let char () = char_of_int (Random.int 256)
  let fixed_string n () = String.init n (fun _ -> char ())
  let string () = fixed_string (Random.int (1 lsl 10)) ()
  let fixed_bytes n () = Bytes.init n (fun _ -> char ())
  let bytes () = fixed_bytes (Random.int (1 lsl 10)) ()

  let fixed_list ?(verbose = false) ?prog n gen () =
    List.init n (fun _ ->
        (match prog with
        | None -> ()
        | Some prog -> if verbose then (prog ()) 1L);
        gen ())

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

let to_json verbose prog inodes : bytes =
  ignore verbose;
  ignore prog;
  ignore inodes;
  failwith "TODO"

(* Bytes.of_string
 *   (String.concat "\n"
 *      (List.map
 *         (fun t ->
 *           if verbose then (prog ()) 1L;
 *           Inter.Val.Serde.from_t t)
 *         inodes)) *)

let run_dataset n inodes_type path verbose =
  let oc = open_out path in
  let progress = progress verbose n in
  Gen.fixed_list ~verbose
    ~prog:(progress `Generating)
    n
    (match inodes_type with
    | `Short -> Gen.short_inode
    | `Long -> Gen.long_inode
    | `Normal -> Gen.inode)
    ()
  |> to_json verbose (progress `Serialising)
  |> output_bytes oc;
  close_out oc

let pre_hash_atom = Irmin.Type.(unstage (pre_hash (pair string Node.value_t)))

let pre_hash_list_atoms =
  Irmin.Type.(unstage (pre_hash (list (pair string Node.value_t))))

let pre_hash ph v =
  let b = Buffer.create 80 in
  ph v (Buffer.add_string b);
  Buffer.contents b

let hash ph = H_contents.hash @@ Bytes.of_string ph

let run_decomp _v =
  let bc = Bytes.of_string "content" in
  let bn = Bytes.of_string "node" in
  let hc = H_contents.hash bc in
  let hn = H_contents.hash bn in
  let sc = "stepc" in
  let sn = "stepn" in
  let content1 = (sc, contents hc) in
  let phc = pre_hash pre_hash_atom content1 in
  let hc = hash phc in
  Format.eprintf "%s@.%a@.@." phc Irmin.Type.(pp H_contents.t) hc;
  let node1 = (sn, node hn) in
  let phn = pre_hash pre_hash_atom node1 in
  let hn = hash phn in
  Format.eprintf "%s@.%a@.@." phn Irmin.Type.(pp H_contents.t) hn;
  let la = [ (* node1 *)
             (* ; content1 *) ] in
  let phl = pre_hash pre_hash_list_atoms la in
  let hl = hash phl in
  Format.eprintf "%s@.%a@.@." phl Irmin.Type.(pp H_contents.t) hl;
  let v = Inter.Val.v la in
  Format.eprintf "%a@." (Irmin.Type.pp_json Inter.Val.t) v

let run action n inodes_type path verbose =
  match action with
  | `Dataset -> run_dataset n inodes_type path verbose
  | `DecompHash -> run_decomp verbose

open Cmdliner

let inodes_number =
  let doc = "Number of inodes to generate." in
  Arg.(value & opt int 1_000 & info [ "n"; "inodes-number" ] ~doc)

let inodes_type =
  let doc = "Normal inode." in
  let normal = (`Normal, Arg.info [ "normal" ] ~doc) in
  let doc = "Short inode." in
  let short = (`Short, Arg.info [ "short" ] ~doc) in
  let doc = "Long inode." in
  let long = (`Long, Arg.info [ "long" ] ~doc) in
  Arg.(value & vflag `Normal [ normal; short; long ])

let path =
  let doc = "The file where the result will be printed." in
  Arg.(value & opt string "inodes.json" & info [ "o"; "output" ] ~doc)

let verbose =
  let doc = "Be verbose." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let action =
  let doc = "Dataset." in
  let dataset = (`Dataset, Arg.info [ "s"; "dataset" ] ~doc) in
  let doc = "Dataset." in
  let hash_decomp = (`DecompHash, Arg.info [ "h"; "decomphash" ] ~doc) in
  Arg.(value & vflag `DecompHash [ dataset; hash_decomp ])

let cmd =
  let doc = "Irmin inodes generation" in
  Term.
    ( const run $ action $ inodes_number $ inodes_type $ path $ verbose,
      info "inodes-gen" ~doc )

let () = Term.(exit @@ eval cmd)
