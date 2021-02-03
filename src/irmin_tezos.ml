include Tezos_storage_encoding.Context

module Conf = struct
  let entries = 32
  let stable_hash = 256
end

module Store =
  Irmin_pack.Make_ext
    (struct
      let io_version = `V1
    end)
    (Conf)
    (Metadata)
    (Contents)
    (Path)
    (Branch)
    (Hash)
    (Node)
    (Commit)
