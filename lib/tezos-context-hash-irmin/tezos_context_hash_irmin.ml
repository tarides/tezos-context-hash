open Encoding
module Encoding = Encoding

module V1 = struct
  let version = `V1
end

module Conf = struct
  let entries = 32
  let stable_hash = 256
end

module Maker = Irmin_pack.Maker_ext (V1) (Conf) (Node) (Commit)
module Store = Maker.Make (Metadata) (Contents) (Path) (Branch) (Hash)
