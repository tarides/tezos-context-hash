type nonrec int = int

let fixed_int i =
  let b = Bytes.create 8 in
  Int64.of_int i |> Bytes.set_int64_be b 0;
  Bytes.unsafe_to_string b

type contents = bytes

let contents b =
  let len = Bytes.length b in
  let len_bytes = fixed_int len in
  let buf = Bytes.create (len + 8) in
  Bytes.blit_string len_bytes 0 buf 0 8;
  Bytes.blit b 0 buf 8 len;
  Bytes.unsafe_to_string buf
