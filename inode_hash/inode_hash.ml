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

module Serde = struct
  type binding = Node.step * Node.value [@@deriving irmin]

  type v =
    | Values of Node.hash * binding list
    | Tree of Node.hash * v option list

  let v_t v_t : v Irmin.Type.t =
    let open Irmin.Type in
    variant "tree" (fun values tree -> function
      | Values (hash, bl) -> values (hash, bl)
      | Tree (hash, iil) -> tree (hash, iil))
    |~ case1 "Values"
         (pair Node.hash_t (list binding_t))
         (fun (h, bl) -> Values (h, bl))
    |~ case1 "Tree"
         (pair Node.hash_t (list (option v_t)))
         (fun (d, t) -> Tree (d, t))
    |> sealv

  let v_t = Irmin.Type.mu @@ fun v -> v_t v

  type s = { hash : Node.hash; bindings : binding list; v : v option }
  [@@deriving irmin]

  let from_it it =
    let rec from_struct_pred = function
      | `Tree (hash, t) ->
          Tree
            ( hash,
              List.map
                (function
                  | None -> None | Some sp -> Some (from_struct_pred sp))
                t )
      | `Values (hash, l) -> Values (hash, l)
    in
    from_struct_pred (Inter.Val.structured_pred it)

  (* Takes a Bin.t and convert it to a Serde.t
   * As a remainder, a Bin.t has the following type
   * type ptr = { index : int; hash : H.t }
   * type tree = { depth : int; length : int; entries : ptr list }
   * type v = Values of (step * value) list | Tree of tree
   * and t (value) = { hash : hash Lazy.t; stable : bool; v : v }
   * To
   * type t = {
   *   hash : H_contents.t;
   *   bindings : (string * Inode.Val.value) list;
   *   tree : serde_tree option;
   * }
   * Bindings can easily be obtained with list
   * tree is of type
   * type tree =
   *   | Values of serde_binding list
   *   | Tree of int * serde_inode option list
   * and inode is of type
   * serde_inode = { hash : H_contents.t; tree : serde_tree; depth : int }
   * ptr is transformed in t for this serialisation so the type we need to
   * serialise is
   * type tree = { depth : int; length : int; entries : t option array }
   * and v = Values of value StepMap.t | Tree of tree
   * and t (value) = { hash : hash Lazy.t; stable : bool; v : v }
   *)
  let from_t (t : Inter.Val.t) =
    let v = Some (from_it t) in
    let bt = Inter.Val.to_bin t in
    Irmin.Type.to_json_string s_t
      { hash = Inter.Elt.hash bt; bindings = Inter.Val.list t; v }

  let to_t s =
    match Irmin.Type.of_json_string s_t s with
    | Ok { bindings; hash = h; _ } ->
        let t = Inter.Val.v bindings in
        if h = Inter.Val.hash t then t
        else failwith "The serialized hash and the computed hash don't match"
    | Error (`Msg e) -> failwith e
end

let to_json verbose prog inodes : bytes =
  Bytes.of_string
    (String.concat "\n"
       (List.map
          (fun t ->
            if verbose then (prog ()) 1L;
            Serde.from_t t)
          inodes))

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
