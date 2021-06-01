open Encoding
module Encoding = Encoding

module V1 = struct
  let version = `V1
end

module Conf = struct
  let entries = 32
  let stable_hash = 256
end

module Maker = Irmin_pack.V1 (Conf)

module Store = struct
  include Maker.Make (Schema)
  module Schema = Schema
end
