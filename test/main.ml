open Monolith

module type Testable = sig
  val fixed_int : int -> string
  val leb128_int : int -> string
  val contents : bytes -> string
end

module C : Testable = Spec

module R : Testable = struct
  open Irmin.Type

  let fixed_int i =
    let buf = Bytes.create 8 in
    unstage (encode_bin int64) (Int64.of_int i) (fun s ->
        Bytes.blit_string s 0 buf 0 8);
    Bytes.unsafe_to_string buf

  let leb128_int i =
    let buf = Buffer.create 8 in
    unstage (encode_bin int) i (Buffer.add_string buf);
    Buffer.contents buf

  let contents b =
    let buf = Buffer.create (8 + Bytes.length b) in
    (unstage (pre_hash Irmin_tezos.Encoding.Contents.t))
      b (Buffer.add_string buf);
    Buffer.contents buf
end

let positive_int = int_within (Gen.int Int.max_int)

let bytes =
  easily_constructible
    (fun () -> Bytes.of_string (Gen.string (Gen.int (1 lsl 20)) Gen.char ()))
    (fun b -> PPrint.string (Bytes.to_string b))

let string = deconstructible PPrint.string
let check_size s = String.length s > 10

let () =
  let spec = positive_int ^> string in
  declare "fixed int" spec R.fixed_int C.fixed_int;

  let spec = positive_int ^> string in
  declare "LEB128 int" spec R.leb128_int C.leb128_int;

  let spec = bytes ^> string in
  declare "contents" spec R.contents C.contents

let () =
  let fuel = 10 in
  main fuel
