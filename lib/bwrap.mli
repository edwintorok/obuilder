include S.SANDBOX
type config [@@deriving sexp]
(** The type of sandbox configurations *)

val ro_root: config
(** [ro_root] mounts the root directory readonly *)

val cmdliner : config Cmdliner.Term.t
(** [cmdliner] is used for command-line interfaces to generate the necessary flags
    and parameters to setup a specific sandbox's configuration. *)

val create : state_dir:string -> config -> t Lwt.t
(** [create ~state_dir config] is a sandboxing system that keeps state in [state_dir]
    and is configured using [config]. *)
