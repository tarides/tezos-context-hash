module Encoding = Encoding
module Conf : Irmin_pack.Conf.S

module Store : sig
  include
    Irmin.S
      with module Schema = Encoding
       and type Private.Remote.endpoint = unit

  val traverse_pack_file :
    [ `Reconstruct_index of [ `In_place | `Output of string ] | `Check_index ] ->
    Irmin.config ->
    unit
end
