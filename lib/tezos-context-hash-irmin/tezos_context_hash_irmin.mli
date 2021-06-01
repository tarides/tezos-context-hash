module Encoding = Encoding
open Encoding
module Conf : Irmin_pack.Conf.S

module Store : sig
  include
    Irmin.S (* FIXME: we just want to "forget" about node equality *)
      with type Schema.hash = Schema.hash
       and type Schema.branch = Schema.branch
       and type Schema.info = Schema.info
       and type Schema.commit = Schema.commit
       and type Schema.metadata = Schema.metadata
       and type Schema.step = Schema.step
       and type Schema.path = Schema.path
       and type Schema.contents = Schema.contents
       and type Private.Remote.endpoint = unit

  val reconstruct_index : ?output:string -> Irmin.config -> unit
end
