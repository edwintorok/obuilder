type t = { root: Fpath.t }

open Sexplib.Conv
type config = unit [@@deriving sexp]

let hosts_of t = Fpath.(t.root / "hosts" |> to_string)

let create ~state_dir _conf =
  let root = Fpath.v state_dir in
  Os.ensure_dir state_dir;
  Lwt.return { root }

let my_user = Unix.getuid (), Unix.getgid ()

let opts_of_user =
  function
  | `Windows _ -> assert false (* not supported *)
  | `Unix Obuilder_spec.{uid; gid} ->
    if (uid, gid) = my_user then []
    else ["--unshare-user"; "--uid"; string_of_int uid; "--gid"; string_of_int gid]


let opts_of_hostname t hostname =
  ["--unshare-uts"; "--hostname"; hostname; "--ro-bind"; hosts_of t ; "/etc/hosts"]

let opts_of_cwd cwd = ["--chdir"; cwd]

let opts_of_argv argv = "--" :: argv

let opts_of_env env =
  "--clearenv" :: List.concat_map (fun (k, v) -> ["--setenv"; k; v]) env


let opts_of_mounts t mounts =
  let open Config.Mount in
  mounts |> List.concat_map @@ fun mount ->
  let src = match mount.ty with
  | `Volume ->
    let path = Fpath.(t.root / "volumes" / mount.src |> to_string) in
    Os.ensure_dir path;
    path
  | `Bind -> mount.src
  | _ -> .
  in
  [ if mount.readonly then "--ro-bind" else "--bind"
  ; src
  ; mount.dst ]

let opts_of_networks = function
  | ["host"] -> ["--ro-bind"; "/etc/resolv.conf"; "/etc/resolv.conf"]
  | _ -> ["--unshare-net"]

let opts_of_secrets lst =
  lst |> List.concat_map @@ fun _secret ->
  failwith "TODO"

(* from sandbox.runc.ml *)

(* This is a subset of the capabilities that Docker uses by default.
   These control what root can do in the container.
   If the init process is non-root, permitted, effective and ambient sets are cleared.
   See capabilities(7) for full details. *)
let default_linux_caps = [
  "CAP_CHOWN";                (* Make arbitrary changes to file UIDs and GIDs *)
  "CAP_DAC_OVERRIDE";         (* Bypass file read, write, and execute permission checks. *)
  "CAP_FSETID";               (* Set SUID/SGID bits. *)
  "CAP_FOWNER";               (* Bypass permission checks. *)
  "CAP_MKNOD";                (* Create special files using mknod. *)
  "CAP_SETGID";               (* Make arbitrary manipulations of process GIDs. *)
  "CAP_SETUID";               (* Make arbitrary manipulations of process UIDs. *)
  "CAP_SETFCAP";              (* Set arbitrary capabilities on a file. *)
  "CAP_SETPCAP";              (* Add any capability from bounding set to inheritable set. *)
  "CAP_SYS_CHROOT";           (* Use chroot. *)
  "CAP_KILL";                 (* Bypass permission checks for sending signals. *)
  "CAP_AUDIT_WRITE"           (* Write records to kernel auditing log. *)
  (* Allowed by Docker, but disabled here (because we use host networking):
  "CAP_NET_RAW";              (* Use RAW and PACKET sockets / bind to any address *)
  "CAP_NET_BIND_SERVICE";     (* Bind a socket to Internet domain privileged ports. *)
  *)
]

let sys_mounts dir =
  [ "--bind"; Filename.concat dir "rootfs"; "/"
  ; "--proc"; "/proc"
  ; "--dev"; "/dev"
  ; "--ro-bind"; "/sys"; "/sys"
  ; "--ro-bind-try"; "/sys/fs/cgroup"; "/sys/fs/cgroup"
  ; "--tmpfs"; "/dev/shm"
  (*; "--mqueue"; "/dev/mqueue" EPERM *)
  ]

let run ~cancelled ?stdin ~log t config dir =
  (* TODO: use dir *)
  let open Config in
  let open Lwt.Syntax in
  (* we are not docker *)
  assert (config.entrypoint = None); 
  let* () = Os.write_file ~path:(hosts_of t) ("127.0.0.1 localhost " ^ config.hostname) in
  let cmd =
    "bwrap" ::
    List.concat
    [ opts_of_user config.user (* switch namespaces early if needed *)
    ; sys_mounts dir (* ensure / is mounted *)
    ; ["--new-session"; "--die-with-parent"]
    ; List.concat_map (fun cap -> ["--cap-add"; cap]) default_linux_caps
    ; opts_of_cwd config.cwd
    ; opts_of_hostname t config.hostname
    ; opts_of_env config.env
    ; opts_of_mounts t config.mounts
    ; opts_of_networks config.network
    ; opts_of_secrets config.mount_secrets
    ; opts_of_argv config.argv (* must be last *)
    ]
  in
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let stdin = Option.map (fun x -> `FD_move_safely x) stdin
  and stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let pp f = Os.pp_cmd f ("", config.argv) in
  let* proc = Os.exec_result ?stdin ~stdout ~stderr ~pp cmd
  and* () = Build_log.copy ~src:out_r ~dst:log in
  if Lwt.is_sleeping cancelled then Lwt.return (proc :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled

let finished _ = Lwt.return_unit

open Cmdliner
let cmdliner : config Term.t =
  Term.(const ())
