type cache = {
  lock : Lwt_mutex.t;
  mutable gen : int;
}

type t =
{ path: string
; caches : (string, cache) Hashtbl.t;
}

let (/) = Filename.concat
let caches path = path / "caches"
let state path = path / "state"

let create ~path () =
  Os.ensure_dir ~mode:0o700 path;
  Os.ensure_dir ~mode:0o700 (caches path);
  Os.ensure_dir ~mode:0o700 (state path);
  { path; caches = Hashtbl.create 10 }

let root t = t.path

let df _ = Os.free_space_percent "/" |> Lwt.return

let build _ ?base:_ ~id:_ fn = fn "/"

let state_dir t = state t.path

let result _ _ = Lwt.return_none

let log_file t id = t.path / "logs" / (id ^ ".log") |> Lwt.return

let delete t id =
  let open Lwt.Syntax in
  let* log = log_file t id in
  let* exists = Lwt_unix.file_exists log in
  if exists then Lwt_unix.unlink log
  else Lwt.return_unit

let cache_paths t name =
  let base = caches t.path / name in
  base, base / "cache", base / "locks"

let counter = Atomic.make 0

let cache ~user t name =
  let open Lwt.Syntax in
  let parent, cache, locks = cache_paths t name in
  let* () = match user with
  | `Windows _ -> assert false
  | `Unix {Obuilder_spec.uid; gid} ->
    match Os.check_dir parent with
    | `Missing ->
      Os.ensure_dir parent;
      Os.ensure_dir cache;
      Os.ensure_dir locks;
      Lwt_unix.chown cache uid gid
    | `Present -> Lwt.return_unit
  in
  let lock = locks / string_of_int (Atomic.fetch_and_add counter 1) in
  Os.ensure_dir lock;
  let release () =
    Lwt_unix.rmdir lock
  in
  Lwt.return (cache, release)

let delete_cache t name =
  let open Lwt.Syntax in
  let parent, cache, locks = cache_paths t name in
  Lwt.try_bind
    (fun () ->
      (* until release is called on all paths inside 'locks' this will fail with
         [ENOTEMPTY] or [EEXIST] (POSIX).
       *)
      Lwt_unix.rmdir locks
    )
    (fun () ->
      let* () = Os.exec ["rm"; "-r"; cache] in
      Lwt_unix.rmdir parent |> Lwt_result.ok
    )
    (function
    | Unix.Unix_error(Unix.(ENOTEMPTY | EEXIST), _, _) ->
      Lwt_result.fail `Busy
    | e -> Lwt.reraise e
    )

let complete_deletes _ = Lwt.return_unit
