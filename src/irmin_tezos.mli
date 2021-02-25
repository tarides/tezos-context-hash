module Hash : sig
  include Irmin.Hash.S

  val to_context_hash : t -> Tezos_base__TzPervasives.Context_hash.t
  val of_context_hash : Tezos_base__TzPervasives.Context_hash.t -> t
end

module Contents : Irmin.Contents.S with type t = bytes
module Metadata : Irmin.Metadata.S with type t = unit
module Path : Irmin.Path.S with type step = string and type t = string list
module Branch : Irmin.Branch.S with type t = string

module Node :
  Irmin.Private.Node.S
    with type hash = Hash.t
     and type step = string
     and type metadata = unit

module Commit : Irmin.Private.Commit.S with type hash = Hash.t
module Conf : Irmin_pack.Config.S

module Store : sig
  include
    Irmin.S
      with type key = Path.t
       and type step = Path.step
       and type metadata = Metadata.t
       and type contents = Contents.t
       and type branch = Branch.t
       and type hash = Hash.t
       and type Private.Sync.endpoint = unit

  val reconstruct_index : ?output:string -> Irmin.config -> unit
end
