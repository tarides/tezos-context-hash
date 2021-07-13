module Encoding = Encoding
open Encoding
module Conf : Irmin_pack.Conf.S

module Store : sig
  include
    Irmin.S
      with type key = Path.t
       and type step = Path.step
       and type metadata = Metadata.t
       and type contents = Contents.t
       and type branch = Branch.t
       and type hash = Hash.t
       and type Private.Remote.endpoint = unit

  val traverse_pack_file :
    [ `Reconstruct_index of [ `In_place | `Output of string ] | `Check_index ] ->
    Irmin.config ->
    unit
end
