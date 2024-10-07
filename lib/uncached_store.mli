(* Do not store build results.

  Does not require root privileges.
  Can use reflinks for implementing [cache], but falls back to a regular copy if unavailable.

  Can also be used where we do not wish to store the files from a build,
  e.g. during `dune runtest` if we rely `dune`'s caching mechanism instead.
*)

include S.STORE

val create: path:string -> unit -> t
(** [create ~path ()] is an uncached store storing logfiles under [path]. *)
