(* 
  We have a sandbox, and unsandboxed part of the pipeline.
  The unsandboxed dune one could be optional, so we don't depend on anything in the user's environment?
*)

module Git = Current_git
open Current.Syntax

type input =
{ spec: Current_git.Commit.t Current.t
; code: Current_git.Commit.t Current.t 
}

(** [commits_of sourcedir] determines the SCM and .spec commits for [sourcedir].
  Currently assumes that .spec is checked out in the parent directory.

  We only work with git commits to catch build failures that would occur
  due to forgotten `git add` commands.

  The commit returned here should never be operated on directly.
  Instead [Current_git.with_checkout] or [with_worktree] should be used instead.

  [Commit_commands] does that automatically.
 *)
let input_of sourcedir =
  let open Current_git in
  let code_repo = Local.v sourcedir in
  let spec_repo = Local.v (Fpath.parent sourcedir) in
  { code = Local.head_commit code_repo
  ; spec = Local.head_commit spec_repo
  }

let config = Current.Config.v ()

open Commands

let command {cwd; args} ~job =
  (* empty string as name means to look argv[0] up in $PATH *)
  let cmd = ("", Array.of_list args) in
  Current.Process.exec ?cwd cmd ~job ~cancellable:true

let specscm spec code ~job =
  let spec, cmd = Repo.SpecSCM.v spec code in
  let open Lwt_result.Syntax in
  let+ () = command cmd ~job in
  spec

(*
let () = Logs.Src.set_level pipeline (Some Logs.Debug)
*)

module CommitKey = struct
  include Current_git.Commit

  let digest t = Current_git.Commit_id.digest (id t)
end

module type Key = sig
  include Current_cache.S.WITH_DIGEST

  val pp : t Fmt.t
end

type 'a key = (module Key with type t = 'a)

module type Value = sig
  include Current_cache.S.WITH_MARSHAL

  val pp : t Fmt.t
end

type 'a value = (module Value with type t = 'a)

let make_step (type key value) (module K : Key with type t = key)
    (module V : Value with type t = value) ~id build =
  let module M = struct
    type t = unit

    let id = id

    module Key = K
    module Value = V

    let build () job key = build job key

    let pp = K.pp

    let auto_cancel = false
  end in
  let module C = Current_cache.Make (M) in
  fun arg ->
    Current.component "%s" id
    |> let> arg = arg in
       C.get () arg

let git ?cwd cmd =
  let cmd =
    ("", "git" :: "-c" :: "protocol.file.allow=always" :: cmd |> Array.of_list)
  in
  Current.Process.exec ~cancellable:true ?cwd cmd

let git_with ?cwd ~path cmd =
  let cmd = "-C" :: Fpath.to_string path :: cmd in
  git ?cwd cmd

let digest_command cmd =
  let cwd = Option.map Fpath.to_string cmd.cwd |> Option.value ~default:"" in
  Filename.quote_command cwd cmd.args

let pp_command =
  Fmt.Dump.(
    record
      [ field "cwd" (fun t -> t.cwd) (option Fpath.pp)
      ; field "args" (fun t -> t.args) (list string) ] )

module Command = struct
  type t = command
  let digest = digest_command
  let pp = pp_command
end

module CommitCommand = struct
  type t = CommitKey.t * command list

  let digest (k, cmd) = CommitKey.digest k ^ String.concat " " (List.map digest_command cmd)

  (* ignore cwd, because that is just a dummy value at this stage *)
  let pp = Fmt.pair CommitKey.pp Fmt.(using (fun t -> t.args) (list ~sep:sp string) |> hovbox |> list ~sep:Fmt.cut |> vbox)
end

module CommitPair = struct
  type t = CommitKey.t * CommitKey.t

  let digest (t1, t2) = CommitKey.digest t1 ^ CommitKey.digest t2

  let pp = Fmt.pair CommitKey.pp CommitKey.pp
end

module FpathKey = struct
  include Fpath

  let digest = Fpath.to_string
end

let get_repo_url =
  let cmd = ("", [|"git"; "ls-remote"; "--get-url"|]) in
  let open Lwt.Syntax in
  make_step (module FpathKey) (module Current.String) ~id:"get url"
  @@ fun job repo ->
  let* () = Current.Job.start ~level:Current.Level.Harmless job in
  Current.Process.check_output ~cwd:repo ~cancellable:true ~job cmd
  |> Lwt_result.map String.trim

let gref_of_head = function
  | `Commit id ->
      Current_git.Commit_id.gref id
  | `Ref ref -> (
    match String.split_on_char '/' ref with
    | "refs" :: "heads" :: gref ->
        String.concat "/" gref
    | _ ->
        ref )

let clone_latest_remote local =
  let open Current_git in
  let* url = get_repo_url (Current.return ~label:"spec repo" @@ Local.repo local)
  and* head = Local.head local in
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_min 1) () in
  clone ~clone_config:fast_config ~schedule ~gref:(gref_of_head head) url

let code_command output ~id ~f commit =
  let open Current_git in
  let cmd_template = f (Repo.Code (Fpath.v "/")) in
  let input = Current.pair commit (Current.return cmd_template) in
  input
  |> make_step (module CommitCommand) output ~id
     @@ fun job (commit, _cmd) ->
     let open Lwt.Syntax in
     let* () = Current.Job.start ~level:Current.Level.Mostly_harmless job in
     Current.Process.with_tmpdir @@ fun tmp ->
     let open Lwt_result.Syntax in
     let* () = command ~job {cwd = Some tmp; args=["git"; "clone"; "-q"; Commit.repo commit |> Fpath.to_string; "clone"]} in
     let code = Repo.Code Fpath.(tmp / "clone") in
     f code |> List.fold_left (fun prev cmd ->
       let* () = prev in
       command ~job cmd
     ) (Lwt_result.return ())

let code_command_unit = code_command (module Current.Unit)

let kojienv_apush ~code_repo ~spec_repo =
  let open Current_git in
  let input =
    Current.pair code_repo (clone_latest_remote spec_repo)
  in
  input
  |> make_step (module CommitPair) (module Commit) ~id:"kojienv apush"
     @@ fun job (code_repo_commit, spec_repo_clone) ->
     let open Lwt.Syntax in
     let code_repo_commit_id = Commit.id code_repo_commit in
     Current.Job.log job "commit_id::: %a" Commit_id.pp code_repo_commit_id;
     let* () = Current.Job.start ~level:Current.Level.Mostly_harmless job in
     with_checkout ~job spec_repo_clone
     @@ fun spec_path ->
     let open Lwt_result.Syntax in
     let branch = "private/edvint/ci" ^ Commit_id.gref code_repo_commit_id in
     let* () =
       git_with ~job ~path:spec_path
         [ "checkout"
         ; "-b"
         ; branch ]
     in
     let* () =
       git_with ~job ~path:(Commit.repo code_repo_commit)
         [ "checkout"
         ; "-b"
         ; branch ]
     in
     let spec = Repo.Spec spec_path in
     let code = Repo.Code (Commit.repo code_repo_commit) in
     let open Lwt_result.Syntax in
     let* spec = specscm spec code ~job in
     let* out = Current.Process.check_output ~job ~cancellable:true ~cwd:Fpath.(spec_path /"scm") ("git", [|"git"; "remote"; "-v"|]) in
     Current.Job.log job "remote: %s" out;
     let* () = command ~job @@ Kojienv.apush ~spec in
     let cmd = ("git", [|"git"; "rev-parse"; "HEAD"|]) in
     let* hash = Current.Process.check_output ~job ~cancellable:true ~cwd:spec_path cmd in
     let repo = spec_repo |> Local.repo |> Fpath.to_string in
     let id = Commit_id.v ~repo ~gref:branch ~hash:(String.trim hash) in
     Commit.v ~repo:(Local.repo spec_repo) ~id
     |> Lwt_result.return 

(*     let cmd = ("", [|"kojiurl"|]) in
     Current.Process.check_output
       ~cwd:Fpath.(spec_path / "SRPM")
       ~cancellable:true ~job cmd*)

let kojiurl spec_commit =
  spec_commit
  |> make_step (module CommitKey) (module Current.String) ~id:"kojiurl"
     @@ fun job commit ->
     let open Lwt.Syntax in
     let* () = Current.Job.start ~level:Current.Level.Harmless job in
     let cmd = ("kojiurl", [|"kojiurl"|]) in
     Current_git.with_checkout ~job commit @@ fun cwd ->
     Current.Process.check_output ~job ~cancellable:true ~cwd:Fpath.(cwd / "SRPM") cmd

let koji_build ~kojitag kojiurl =
  kojiurl
  |> make_step (module Current.String) (module Current.String) ~id:"koji build"
     @@ fun job kojiurl ->
     let open Lwt.Syntax in
     let* () = Current.Job.start ~level:Current.Level.Average job in
     let cmd = ("koji", [|"koji"; "build"; "--wait"; kojitag; kojiurl|]) in
     let open Lwt_result.Syntax in
     let* () = Current.Process.exec ~job ~cancellable:true cmd in
     let cmd =
       ( "kapi"
       , [|"kapi"; "--regen-repo"; "--suffix"; "toolstack" (* TODO... *)|] )
     in
     Current.Process.check_output ~job ~cancellable:true cmd

let origin local =
  let open Current_git in
  let repo = Local.repo local |> Fpath.to_string in
  (Current.return repo) |> make_step (module Current.String) (module Current.String) ~id:"origin"
  @@ fun job repo ->
  let open Lwt.Syntax in
   let* () = Current.Job.start ~level:Current.Level.Harmless job in
   let cmd = ("git", [|"git"; "remote"; "get-url"; "origin"|]) in
   Current.Process.check_output ~cwd:(Local.repo local) ~job ~cancellable:true cmd
   |> Lwt_result.map String.trim

let with_origin ~origin id =
  let open Current_git in
  Commit_id.v ~repo:origin ~gref:(Commit_id.gref id) ~hash:(Commit_id.hash id)


(** [configure commit] runs './configure' for [commit].
 This ensures that certain files are generated.
*)
let configure (Repo.Code code) =
  let open Current_git in
    [{  cwd = Some code
    ; args = ["./configure"]

    }]

let dune_pipeline commit =
  let cmd ?(gated=commit) id commands = 
    let f commit =
      commands |> List.concat_map @@ fun f -> f commit
    in
    code_command_unit ~id ~f commit
  in

  (* we probably want something like:
   parallel:
     * @fmt
     * @lint
     * @check
     parallel:
     --> gate: @doc
     --> gate: @default --> gate: @install, (using same build dir as default?)
     --> gate: @runtest

   Need some helpers for running commands this way,
   but avoid using monads, need to build pairs, and then use map to remove one?
   or better yet have the command always take some kind of stage param?
  *)

  let typecheck = cmd "typecheck" [Dune.check] in
  let gated = Current.gate ~on:typecheck commit in
  
  gated, Current.all
  [ cmd "format" [Dune.fmt]
  ; cmd "lint" [Dune.lint]
  ; typecheck
  ; cmd ~gated "build doc" [Dune.doc]
  ; cmd ~gated "compile" [configure; Dune.default]
  ; cmd ~gated "install" [configure; Dune.install]
  ; cmd ~gated "runtest" [configure; Dune.runtest]
  ]

let koji_pipeline spec_repo commit =
  let spec_commit = kojienv_apush ~code_repo:commit ~spec_repo in
  let url = kojiurl spec_commit in
  let kojitag = "pb-edvint-toolstack" in
  koji_build ~kojitag url
  |> Current.ignore_value

let repo_pipeline dir =
  let open Current_git in
  let code_repo = Local.v dir in
  let spec_repo = Local.v (Fpath.parent dir) in

  let commit = local_clone code_repo in
  let gated, dune = dune_pipeline commit in
  Current.all
  [ dune
  ; koji_pipeline spec_repo commit
  ]

(*  let kojitag = "pb-edvint-toolstack" in
  
  (* TODO: determine automatically or use cmd param *)
  Current.all
    [ dune_pipeline
    ; (let kojiurl = kojienv_apush ~code_repo ~spec_repo in
       koji_build ~kojitag kojiurl
       (* TODO: deploy/run tests *)
       |> Current.ignore_value ) ]*)

let v ~repodirs () = repodirs |> List.map repo_pipeline |> Current.all
