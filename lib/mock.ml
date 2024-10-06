type t =
{ root: Fpath.t
; config: string
}

let create ~config root = { root; config }

let root t = Fpath.to_string t.root

let df t = t |> root|> Lwt_preemptive.detach Os.free_space_percent

let log_file t id = Lwt.return Fpath.(t.root / "logs" / (id ^ ".log") |> to_string)

let state_dir t = Fpath.(t.root / "state" |> to_string)

(* TODO: had to disable modprobe in overlayfs.py when run inside distrobox *)
(* TODO: had to add optionsArg += ",userxattr" in overlayfs.py *)
let mock_cmd t args =
  let base_dir = Fpath.(t.root / "overlayfs_base_dir") in
    ["mock"; "-r"; t.config; "--enable-plugin"; "overlayfs"; "--plugin-option"; "overlayfs:base_dir=" ^ Fpath.to_string base_dir; "--plugin-option"; "overlayfs:touch_rpmdb=True"; "--disable-plugin"; "root_cache"; "--disable-plugin"; "lvm_root"; "--no-bootstrap-chroot"; "-q" ] @ args

let mock t args = mock_cmd t args |> Os.exec

open Lwt.Syntax

let result t id = 
  let is_id line =
    let line =
      if String.starts_with ~prefix:"* " line then
        String.sub line 2 (String.length line - 2)
      else line
    in
    String.equal id line
  in
  let+ snapshots = mock_cmd t ["--list-snapshots"] |> Os.pread in
  let has = String.split_on_char '\n' snapshots |> List.exists is_id in
  if has then
    Some t.config
  else None



let build t ?(base="postinit") ~id fn =
  (* TODO: the "tempdir" needs to be independent, so we can run multiple of these concurrently,
     but that is not how mock works....
   *)
  let* () = mock t ["--rollback-to"; base] in
  Lwt.try_bind
    (fun () -> fn t.config)
    (fun r ->
       let+ () = mock t ["--snapshot"; id] in
       r
    )
  (fun ex ->
      Log.warn (fun f -> f "Uncaught exception from %S build function: %a" id Fmt.exn ex);
      let* () = mock t ["--clean"] in
      Lwt.reraise ex
  )

let delete t id =
  mock t ["--remove-snapshot"; id]

let complete_deletes _ = Lwt.return_unit

(* TODO: lost it all.. *)

(* cache: create and call Overlay *)
