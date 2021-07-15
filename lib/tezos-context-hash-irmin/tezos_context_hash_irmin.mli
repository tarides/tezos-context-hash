module Encoding = Encoding
open Encoding
module Conf : Irmin_pack.Conf.S

module Store : sig
  include
    Irmin.S
      with module Schema = Encoding
       and type Private.Remote.endpoint = unit

  val reconstruct_index : ?output:string -> Irmin.config -> unit
end
