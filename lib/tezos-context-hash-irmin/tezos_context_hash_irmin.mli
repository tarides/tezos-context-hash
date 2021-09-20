module Encoding = Encoding
module Conf : Irmin_pack.Conf.S

module Store : sig
  include
    Irmin.Generic_key.S
      with type Schema.Hash.t = Encoding.Hash.t
       and type Schema.Branch.t = Encoding.Branch.t
       and type Schema.Metadata.t = Encoding.Metadata.t
       and type Schema.Path.t = Encoding.Path.t
       and type Schema.Path.step = Encoding.Path.step
       and type Schema.Contents.t = Encoding.Contents.t
       and type Backend.Remote.endpoint = unit

  val traverse_pack_file :
    [ `Reconstruct_index of [ `In_place | `Output of string ]
    | `Check_index
    | `Check_and_fix_index ] ->
    Irmin.config ->
    unit
end
