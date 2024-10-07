open Sexplib.Conv

type commands = Obuilder_spec.op list [@@deriving sexp_of]

let command cwd args ~job =
  (* empty string as name means to look argv[0] up in $PATH *)
  let cmd = ("", Array.of_list args) in
  Current.Process.exec ?cwd cmd ~job ~cancellable:true

let result_iter_s f lst =
  let open Lwt_result.Syntax in
  List.fold_left
    (fun prev current ->
      let* () = prev in
      f current )
    (Lwt_result.return ()) lst

(* TODO: upstream *)
let with_worktree ?pool ~job commit (fn : Fpath.t -> 'a Current.or_error Lwt.t)
    : 'a Current.or_error Lwt.t =
  let open Current_git in
  let repo = Commit.repo commit and id = Commit.id commit in
  let short_hash = Astring.String.with_range ~len:8 (Commit_id.hash id) in
  Current.Job.log job "@[<v2>Checking out commit %s. To reproduce:@,%a@]"
    short_hash Commit_id.pp_user_clone id ;
  let switch = Current.Switch.create ~label:"worktree-add" () in
  let open Lwt_result.Syntax in
  Lwt.finalize
    (fun () ->
      let* () =
        match pool with
        | Some pool ->
            Current.Job.use_pool ~switch job pool |> Lwt_result.ok
        | None ->
            Lwt_result.return ()
      in
      Current.Process.with_tmpdir ~prefix:"git-worktree"
      @@ fun tmpdir ->
      let* () =
        command ~job None
          [ "git"
          ; "-C"
          ; Fpath.to_string repo
          ; "worktree"
          ; "add"
          ; "--detach"
          ; Fpath.to_string tmpdir
          ; Commit_id.hash id ]
      in
      let* () = Current.Switch.turn_off switch |> Lwt_result.ok in
      fn tmpdir )
    (fun () -> Current.Switch.turn_off switch)

let make ?(level = Current.Level.Mostly_harmless) ~id =
  let module M = struct
    type t = unit

    let id : string = id

    module Key = struct
      type t = Current_git.Commit.t * commands

      let digest (commit, commands) =
        Current_git.(commit |> Commit.id |> Commit_id.digest)
        ^ (commands |> sexp_of_commands |> Sexplib.Sexp.to_string_mach)
    end

    module Value = Current_git.Commit

    let build () job ((commit, commands) : Key.t) =
      let open Lwt.Syntax in
      let* () = Current.Job.start ~level job in
      with_worktree ~job commit
      @@ fun path ->
      let open Obuilder_spec in
      let commands = workdir (Fpath.to_string path) :: commands in
      let cwd = ref (Fpath.v "/") in
      let env = ref [] in
      let open Lwt_result.Syntax in
      let exec cmds =
        let cmds =
          match !env with
          | [] ->
              cmds
          | lst ->
              "env"
              :: (List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) lst @ cmds)
        in
        Current.Process.exec ~cwd:!cwd
          ("", Array.of_list cmds)
          ~job ~cancellable:true
      in
      let shell_cmds = ref ["/bin/sh"; "-c"] in
      let+ () =
        commands
        |> result_iter_s
           @@ function
           | `Comment _ ->
               Lwt_result.return ()
           | `Workdir dir ->
               cwd := Fpath.v dir ;
               Lwt_result.return ()
           | `Shell cmds ->
               shell_cmds := cmds ;
               Lwt_result.return ()
           | `Run run ->
               exec (!shell_cmds @ [run.shell])
               (* ignore cache, network and secrets: assume already present on host *)
           | `Copy _ | `User _ ->
               (* ignore *) Lwt_result.return ()
           | `Env (k, v) ->
               env := (k, v) :: !env ;
               Lwt_result.return ()
      in
      commit

    let pp ppf ((commit, _) : Key.t) =
      Fmt.pf ppf "Test %a: %s" Current_git.Commit.pp commit id

    let auto_cancel = false
  end in
  let module C = Current_cache.Make (M) in
  fun arg ->
    let open Current.Syntax in
    Current.component "%s" id
    |> let> arg = arg in
       C.get () arg
