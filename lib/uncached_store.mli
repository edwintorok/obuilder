(* Do not store build results.

  Does not require root privileges, and provides pass-through access to caches.
  (applications need to use their own locking mechanisms if needed,
   just as they would if called directly by the user)

  Can also be used where we do not wish to store the files from a build,
  e.g. during `dune runtest` if we rely `dune`'s caching mechanism instead.
*)

include S.STORE

val create: path:string -> unit -> t
(** [create ~path ()] is an uncached store storing logfiles under [path]. *)
