(*
   We have a sandbox, and unsandboxed part of the pipeline.
   The unsandboxed dune one could be optional, so we don't depend on anything in the user's environment?
*)

module Git = Current_git
open Current.Syntax

module Input = struct
  open Current_git

  type t = {spec: Tree.t; code: Tree.t}

  let v ~spec ~code = {spec; code}

  let spec t = t.spec

  let code t = t.code

  let digest t =
    Printf.sprintf "%s %s" (Tree.digest t.spec) (Tree.digest t.code)

  let pp = Fmt.(record [field "spec" spec Tree.pp; field "code" code Tree.pp])
end

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
  let code = Local.head_commit code_repo |> tree_hash
  and spec = Local.head_commit spec_repo |> tree_hash in
  ( code
  , Current.pair spec code
    |> Current.map (fun (spec, code) -> Input.v ~spec ~code) )

open Commands

let command {cwd; args} ~job =
  (* empty string as name means to look argv[0] up in $PATH *)
  let cmd = ("", Array.of_list args) in
  Current.Job.log job "Executing in %a" Fmt.(option Fpath.pp) cwd ;
  Current.Process.exec ?cwd cmd ~job ~cancellable:true

let with_specscm input ~job fn =
  let open Current_git in
  let spec = input |> Input.spec |> Tree.commit
  and code = input |> Input.code |> Tree.commit in
  Commit_commands.with_worktree ~job spec
  @@ fun spec ->
  Commit_commands.with_worktree ~job code
  @@ fun code ->
  let spec, cmd = Repo.(SpecSCM.v (Spec spec) (Code code)) in
  let open Lwt_result.Syntax in
  let* () = command cmd ~job in
  fn spec

let config = Current.Config.v ()

(*
let () = Logs.Src.set_level pipeline (Some Logs.Debug)
*)

module type Key = sig
  include Current_cache.S.WITH_DIGEST

  val pp : t Fmt.t
end

module Pair (A : Key) (B : Key) = struct
  type t = A.t * B.t

  let digest (a, b) = String.concat " " [A.digest a; B.digest b]

  let pp = Fmt.pair ~sep:Fmt.sp A.pp B.pp
end

type 'a key = (module Key with type t = 'a)

module type Value = sig
  include Current_cache.S.WITH_MARSHAL

  val pp : t Fmt.t
end

type 'a value = (module Value with type t = 'a)

let make_step (type key value) (module K : Key with type t = key)
    (module V : Value with type t = value) ?schedule ~id ~level build =
  let module M = struct
    type t = unit

    let id = id

    module Key = K
    module Value = V

    let build () job key =
      let open Lwt.Syntax in
      let* () = Current.Job.start ~level job in
      build job key

    let pp = K.pp

    let auto_cancel = false
  end in
  let module C = Current_cache.Make (M) in
  fun arg ->
    Current.component "%s" id
    |> let> arg = arg in
       C.get ?schedule () arg

let git ?cwd cmd =
  let cmd =
    ("", "git" :: "-c" :: "protocol.file.allow=always" :: cmd |> Array.of_list)
  in
  Current.Process.exec ~cancellable:true ?cwd cmd

let git_with ?cwd ~path cmd =
  let cmd = "-C" :: Fpath.to_string path :: cmd in
  git ?cwd cmd

let prefix = "private/edvint/ci/"

(* don't use kojienv apush, it depends on whether remote branch exists or not,
   and we want to avoid conditional code like that
*)
let specsync ~name input =
  let open Current_git in
  let commit =
    input
    |> make_step
         (module Input)
         (module Commit)
         ~level:Current.Level.Mostly_harmless ~id:("specsync " ^ name)
       @@ fun job input ->
       with_specscm input ~job
       @@ fun spec ->
       let open Lwt_result.Syntax in
       let* () = command ~job @@ Commands.Xenpkg.specsync ~spec in
       let (Spec path) = (spec :> Repo.spec * Repo.code) |> fst in
       let+ line =
         Current.Process.check_output ~cancellable:true ~job ~cwd:path
           ("", [|"git"; "rev-parse"; "HEAD"|])
       in
       let spec_commit = Input.spec input |> Tree.commit in
       let spec_id = spec_commit |> Commit.id in
       let id =
         Commit_id.v ~repo:(Commit_id.repo spec_id)
           ~gref:(Commit_id.gref spec_id) ~hash:(String.trim line)
       in
       Commit.v ~repo:(Commit.repo spec_commit) ~id
  in
  Current.pair input (commit |> tree_hash)
  |> Current.map @@ fun (input, spec) -> {input with Input.spec}

let mockop ~kojitag ~phase ~cwd args =
  { cwd
  ; args=
      [ "kojienv"
      ; "mockop"
      ; kojitag
      ; "--resultdir=SRPM/build" ^ phase
      ; "--no-clean"
      ; "--no-cleanup-after" ]
      @ args }

let rpm ~kojitag srpm =
  srpm
  |> make_step
       (module Current.String)
       (module Current.Unit)
       ~level:Current.Level.Average ~id:"rpmbuild"
     @@ fun job srpm ->
     let open Lwt_result.Syntax in
     command ~job @@ mockop ~kojitag ~phase:"rpm" ~cwd:None ["--rebuild"; srpm]
(* TODO: returnname? for now just success/fail *)

(* don't use kojienv apush, it depends on whether remote branch exists or not,
   and we want to avoid conditional code like that
*)
let apush input_path =
  let open Current_git in
  input_path
  |> make_step
       (module Input)
       (module Current.String)
       ~level:Current.Level.Average ~id:"apush"
     @@ fun job input ->
     let commit = input |> Input.spec |> Tree.commit in
     Commit_commands.with_worktree ~job commit
     @@ fun path ->
     let gref =
       input |> Input.code |> Tree.commit |> Commit.id |> Commit_id.gref
     in
     let branch = prefix ^ gref in
     let open Lwt_result.Syntax in
     let* () = git_with ~job ~path ["checkout"; "-b"; branch] in
     let* () =
       git_with ~job ~path
         ["commit"; "--allow-empty"; "-a"; "-m"; "pipeline autobuild commit"]
     in
     let* () = git_with ~job ~path ["push"; "origin"; "--force"; branch] in
     Current.Process.check_output ~job ~cancellable:true
       ~cwd:Fpath.(path / "SRPM")
       ("", [|"kojiurl"|])

(** We need a Kerberos token to build anything remotely,
    keep the token that we have alive, or inform the user about its expiration.
 *)
let krenew =
  let valid_for = Duration.of_min 10 in
  let schedule = Current_cache.Schedule.v ~valid_for () in
  make_step
    (module Current.Unit)
    (module Current.String)
    ~schedule ~level:Current.Level.Mostly_harmless ~id:"krenew"
  @@ fun job () ->
  let klist = {cwd= None; args= ["klist"; "-fe"]} in
  (* Renew if less than N minutes lifetime remaining.
   *)
  let krenew =
    { cwd= None
    ; args=
        [ "env"
        ; "KRB5_TRACE=/dev/stderr"
        ; "krenew"
        ; "-H"
        ; 2 * Duration.to_min valid_for |> string_of_int
        ; "-v" ] }
  in
  let open Lwt_result.Syntax in
  (* for debugging *)
  let* () = command ~job klist in
  let* () = command ~job krenew in
  Current.Process.check_output ~job ~cancellable:true
    ("", klist.args |> Array.of_list)

(* TODO: ... typed *)
let kojihub =
  let valid_for = Duration.of_min 5 in
  let schedule = Current_cache.Schedule.v ~valid_for () in
  make_step
    (module Current.String)
    (module Current.Unit)
    ~level:Current.Level.Mostly_harmless ~schedule ~id:"kojihub"
  @@ fun job _klist ->
  command ~job
    {cwd= None; args= ["env"; "KRB5_TRACE=/dev/stderr"; "koji"; "moshimoshi"]}

let bbtoken =
  make_step
    (module Current.Unit)
    (module Current.String)
    ~level:Current.Level.Harmless ~id:"bbtoken"
  @@ fun job () ->
  let open Lwt.Syntax in
  let home = Sys.getenv "HOME" in
  let path = Filename.concat home ".xenpkg/bbtoken" in
  let+ exists = Lwt_unix.file_exists path in
  if exists then Ok path
  else
    Fmt.error_msg
      "%s missing. See \
       https://info.citrite.net/display/xenserver/Using+the+Koji+Production+Instance#UsingtheKojiProductionInstance-Generating$HOME/.xenpkg/bbtokenmanually"
      path

let kojitag hub tag =
  Current.gate ~on:hub tag
  |> make_step
       (module Current.String)
       (module Current.String)
       ~level:Current.Level.Mostly_harmless ~id:"koji taginfo"
     @@ fun job tag ->
     (* check for presence of the tag, and print some configuration information about it *)
     let open Lwt_result.Syntax in
     let* () = command ~job {cwd= None; args= ["koji"; "taginfo"; tag]} in
     let+ () =
       command ~job {cwd= None; args= ["koji"; "list-tag-inheritance"; tag]}
     in
     (* kapi --list-tags --json and check that we have a build target *)
     tag

let kojienvs = Current.state_dir "kojienv"

let koji_path ~kojitag repo = Fpath.(kojienvs / kojitag / repo)

let kojienv_for ~kojitag repo =
  let open Current_git in
  repo
  |> Current.map (fun t ->
         t |> Tree.commit |> Commit.repo |> Fpath.basename |> koji_path ~kojitag
         |> Fpath.to_string )
  |> make_step
       (module Current.String)
       (module Current.String)
       ~level:Current.Level.Average ~id:"kojienv"
     @@ fun job strpath ->
     let open Lwt_result.Syntax in
     Current.Job.log job "kojipath is %s" strpath ;
     let* () = command ~job {cwd= None; args= ["mkdir"; "-p"; strpath]} in
     let+ () =
       command ~job
         {cwd= Some (Fpath.v strpath); args= ["kojienv"; "new"; kojitag]}
     in
     strpath

let kojienv_update ~kojitag path =
  let schedule = Current_cache.Schedule.v ~valid_for:Duration.(of_min 10) () in
  path
  |> make_step
       (module Current.String)
       (module Current.Unit)
       ~level:Current.Level.Average ~schedule ~id:"kojienv"
     @@ fun job strpath ->
     command ~job
       {cwd= Some (Fpath.v strpath); args= ["kojienv"; "update"; kojitag]}

let kojienv ~kojitag input =
  let open Current_git in
  let spec = Current.map Input.spec input in
  let kojipath = kojienv_for ~kojitag spec in
  (*let on = kojienv_update ~kojitag kojipath in*)
  (input, (*Current.gate ~on*) kojipath)

let with_kojienv ~id ~level ~kojitag (input, kojipath) fn =
  let open Current_git in
  let spec = Current.map Input.spec input in
  let spec_repo =
    spec
    |> Current.map (fun t -> t |> Tree.commit |> Commit.repo |> Fpath.basename)
  in
  let module K = Pair (Pair (Tree) (Current.String)) (Current.String) in
  Current.(pair (pair spec kojipath) spec_repo)
  |> make_step (module K) (module Current.String) ~level ~id
     @@ fun job ((tree, kojipath), basen) ->
     let commit = Tree.commit tree in
     Commit_commands.with_worktree ~job commit
     @@ fun path ->
     Current.Job.log job "kojipath is %s" kojipath ;
     let kojipath = Fpath.v kojipath in
     let open Lwt_result.Syntax in
     let* () =
       command ~job
         {cwd= Some kojipath; args= ["ln"; "-fs"; Fpath.to_string path; basen]}
     in
     fn ~job ~kojitag kojipath

let srpm ~kojitag input_path =
  let open Current_git in
  with_kojienv ~level:Current.Level.Average ~kojitag ~id:"srpm" input_path
  @@ fun ~job ~kojitag cwd ->
  let open Lwt_result.Syntax in
  let* () =
    command ~job
    @@ mockop ~kojitag ~phase:"srpm" ~cwd:(Some cwd)
         ["--buildsrpm"; "--spec"; "*.spec"; "--sources"; "SRPM"]
  in
  let+ line =
    Current.Process.check_output ~cwd ~cancellable:true ~job
      ("", [|"find"; "SRPM/build-srpm"; "-name"; "*.src.rpm"|])
  in
  String.trim line

let kojienv_installdeps ~kojitag input_path =
  let open Current_git in
  (* note: perhaps roll back and remove? otherwise won't test missing deps *)
  with_kojienv ~id:"installdeps" ~level:Current.Level.Average ~kojitag
    input_path
  @@ fun ~job ~kojitag codepath ->
  let basen = Fpath.basename codepath in
  let specdir = Fpath.(codepath / basen) in
  let open Lwt.Syntax in
  let* spec =
    Lwt_unix.files_of_directory (Fpath.to_string specdir)
    |> Lwt_stream.find (String.ends_with ~suffix:".spec")
  in
  let open Lwt_result.Syntax in
  let+ () =
    command ~job
      { cwd= Some codepath
      ; args=
          [ "kojienv"
          ; "installdeps"
          ; kojitag
          ; Filename.concat basen (Option.get spec) ] }
  in
  Fpath.to_string codepath

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

module TreeCommand = struct
  open Current_git

  type t = Tree.t * command list

  let digest (k, cmd) =
    Tree.digest k ^ String.concat " " (List.map digest_command cmd)

  (* ignore cwd, because that is just a dummy value at this stage *)
  let pp =
    Fmt.pair Tree.pp
      Fmt.(
        using (fun t -> t.args) (list ~sep:sp string)
        |> hovbox |> list ~sep:Fmt.cut |> vbox )
end

let tree_command output ~level ~id ~f tree =
  let open Current_git in
  let cmd_template = f (Repo.Code (Fpath.v "/")) in
  let input = Current.pair tree (Current.return cmd_template) in
  input
  |> make_step (module TreeCommand) output ~level ~id
     @@ fun job (tree, _cmd) ->
     (* TODO: use worktree *)
     let open Lwt.Syntax in
     let commit = Tree.commit tree in
     Current.Process.with_tmpdir
     @@ fun tmp ->
     let open Lwt_result.Syntax in
     let* () =
       command ~job
         { cwd= Some tmp
         ; args=
             [ "git"
             ; "clone"
             ; "-q"
             ; Commit.repo commit |> Fpath.to_string
             ; "clone" ] }
     in
     let code = Repo.Code Fpath.(tmp / "clone") in
     f code
     |> List.fold_left
          (fun prev cmd ->
            let* () = prev in
            command ~job cmd )
          (Lwt_result.return ())

let tree_command_unit = tree_command (module Current.Unit)

let koji_build ~kojitag kojiurl =
  kojiurl
  |> make_step
       (module Current.String)
       (module Current.String)
       ~level:Current.Level.Average ~id:"koji build"
     @@ fun job kojiurl ->
     let cmd = ("koji", [|"koji"; "build"; "--wait"; kojitag; kojiurl|]) in
     let open Lwt_result.Syntax in
     let* () = Current.Process.exec ~job ~cancellable:true cmd in
     let cmd =
       ( "kapi"
       , [|"kapi"; "--regen-repo"; "--suffix"; "toolstack" (* TODO... *)|] )
     in
     Current.Process.check_output ~job ~cancellable:true cmd

(*let koji_pipeline input =
  let kojitag = "pb-edvint-toolstack" in
  let hub = Current.return () |> krenew |> kojihub in
  let specs = input |> specsync in
  let env = kojienv ~kojitag specs in
  let kojicode = env |> kojienv_installdeps ~kojitag in
  let srpm_input = fst env, kojicode in
  Current.all
  [ specs |> apush |> Current.gate ~on:hub |> koji_build ~kojitag |> Current.ignore_value
  ; srpm_input |> srpm ~kojitag |> rpm ~kojitag
  ]*)

let srpm env_tree =
  let open Current_git in
  let module K = Pair (Kojienv.KojiEnvPath) (Input) in
  env_tree
  |> ( make_step
         (module K)
         (module Current.String)
         ~id:"srpm" ~level:Current.Level.Average
     @@ fun job (env, input) ->
     let open Kojienv.KojiEnvPath in
     let tree = Input.spec input in
     let commit = Tree.commit tree in
     let tree_hash = Tree.tree_hash tree in
     let path = Fpath.(env.path / tree_hash) in
     (* TODO: rm at the end? *)
     Commit_commands.with_worktree' ~job commit path
     @@ fun path ->
     let code_commit = input |> Input.code |> Tree.commit in
     let codepath = Fpath.(path / "scm") in
     Commit_commands.with_worktree' ~job code_commit codepath
     @@ fun _codepath ->
     (* TODO: xenpkg makesources *)
     Current.Job.log job "checked out" ;
     let env = {env with path} in
     let open Lwt_result.Syntax in
     let+ path = Kojienv.srpm ~job env in
     Fpath.to_string path )
  |> Current.map Fpath.v

let build_requires env_spec =
  let open Current_git in
  let module K = Pair (Kojienv.KojiEnvPath) (Tree) in
  env_spec
  |> make_step
       (module K)
       (module Kojienv.BuildRequires)
       ~id:"BuildRequires" ~level:Current.Level.Average
     @@ fun job (env, spec) ->
     let open Kojienv.KojiEnvPath in
     let commit = Tree.commit spec in
     let tree_hash = Tree.tree_hash spec in
     let path = Fpath.(env.path / tree_hash) in
     (* TODO: rm at the end? *)
     Commit_commands.with_worktree' ~job commit path
     @@ fun path ->
     let env = {env with path} in
     Kojienv.build_requires ~job env

let rpmbuild env srpm =
  let open Current_git in
  let env_input = Current.pair env srpm in
  let module K = Pair (Kojienv.KojiEnvPath) (Current.String) in
  env_input
  |> ( make_step
         (module K)
         (module Current.String)
         ~id:"rpmbuild" ~level:Current.Level.Average
     @@ fun job (env, srpm) ->
     let open Lwt_result.Syntax in
     let+ path = Kojienv.rpmbuild ~job env srpm in
     Fpath.to_string path )
  |> Current.map Fpath.v

module Rpms = struct
  (* sometimes the order matter, e.g. for chain build, use a list for now *)
  type t = Fpath.t list

  let marshal t = t |> List.map Fpath.to_string |> String.concat "\n"

  let unmarshal t = t |> String.split_on_char '\n' |> List.map Fpath.v

  let digest = marshal

  let pp = Fmt.(Dump.list Fpath.pp)
end

let chain env srpms =
  let open Current_git in
  let env_input = Current.pair env srpms in
  let module K = Pair (Kojienv.KojiEnvPath) (Rpms) in
  env_input
  |> make_step (module K) (module Rpms) ~id:"chain" ~level:Current.Level.Average
     @@ fun job (env, srpms) ->
     let open Lwt_result.Syntax in
     let+ paths = Kojienv.chain ~job env (srpms |> List.map Fpath.to_string) in
     paths

let rsync ~host files =
  files
  |> make_step
       (module Rpms)
       (module Rpms)
       ~id:"rsync" ~level:Current.Level.Dangerous
     @@ fun job rpms ->
     let cmd =
       [ "rsync"
       ; "--partial"
       ; "--info=progress2"
       ; "-y"
       ; "-e"
       ; "ssh -o StrictHostKeyChecking=no -l root" ]
       @ (rpms |> List.map Fpath.to_string)
       @ [host ^ ":"]
     in
     (*let cmd = ["sh"; "-c"; Filename.quote_command "rsync" cmd; "2>&1"] in*)
     let open Lwt_result.Syntax in
     let+ () =
       Current.Process.exec ~cancellable:true ~job ("", Array.of_list cmd)
     in
     List.map Fpath.base rpms

let deploy ~host rpms =
  (*    List.map Filename.basename rpms @*)
  let cmd =
    rpms |> rsync ~host
    |> Current.map (fun rpms ->
           let rpms = rpms |> List.map Fpath.to_string in
           ["yum"; "info"]
           @ List.map Filename.chop_extension rpms
           @ ["||"; "(yum"; "downgrade"; "-y"]
           @ rpms
           @ ["&&"; "xe-toolstack-restart)"] )
  in
  Current_ssh.run
    ~schedule:(Current_cache.Schedule.v ())
    ~key:"deploy" ("root@" ^ host) cmd

let make_test () =
  Current.return ()
  |> make_step
       (module Current.Unit)
       (module Current.Unit)
       ~id:"debug" ~level:Current.Level.Harmless
     @@ fun job () ->
     let open Lwt_result.Syntax in
     let* () =
       Current.Process.exec ~cancellable:true ~job
         ("", [|"sh"; "-c"; "echo foo >&2; echo bar"|])
     in
     let+ (_ : string) =
       Current.Process.check_output ~cancellable:true ~job
         ("", [|"sh"; "-c"; "echo foo >&2; echo bar"|])
     in
     ()

let koji_pipeline' name input =
  (* TODO: eventually we should createrepo and add the repo on our own,
     for now use a shared kojienv for the chainbuild: the kojienv of the first one,
     this is not quite accurate
  *)
  let kojitag = "pb-edvint-toolstack" in
  let kojitag = Koji.kojitag kojitag in
  let env = Kojienv.for_srpm_build kojitag in
  let br = build_requires (Current.pair env (Current.map Input.spec input)) in
  let env' = Kojienv.cached_or_make kojitag br in
  let specs = input |> specsync ~name in
  let env_input = Current.pair env specs in
  (env', srpm env_input)

let koji_pipeline'' envs_srpms =
  let env' = List.hd envs_srpms |> fst in
  let rpms = chain env' (List.map snd envs_srpms |> Current.list_seq) in
  let host = "lcy2-dt72.xenrt.citrite.net" in
  Current.all [deploy ~host rpms; Current.ignore_value env']
(* TODO: deploy local, deploy remote? *)

(*let specs = input |> specsync in
  let kojicode = env |> kojienv_installdeps ~kojitag in
  let srpm_input = fst env, kojicode in
  Current.all
  [ specs |> apush |> Current.gate ~on:hub |> koji_build ~kojitag |> Current.ignore_value
  ; srpm_input |> srpm ~kojitag |> rpm ~kojitag
  ]*)

(** [configure commit] runs './configure' for [commit].
 This ensures that certain files are generated.
*)
let configure (Repo.Code code) =
  let open Current_git in
  [{cwd= Some code; args= ["./configure"]}]

let dune_pipeline commit =
  let cmd ?(gated = commit) id commands =
    let f commit = commands |> List.concat_map @@ fun f -> f commit in
    tree_command_unit ~level:Current.Level.Mostly_harmless ~id ~f commit
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
  ( typecheck
  , Current.all
      [ cmd "format" [Dune.fmt]
      ; cmd "lint" [Dune.lint]
      ; typecheck
      ; cmd ~gated "build doc" [Dune.doc]
      ; cmd ~gated "compile" [configure; Dune.default]
      ; cmd ~gated "install" [configure; Dune.install]
      ; cmd ~gated "runtest" [configure; Dune.runtest] ] )

let repo_pipeline dirs =
  let open Current_git in
  dirs
  |> ( List.map
     @@ fun dir ->
     let _code, input = input_of dir in
     koji_pipeline' (Fpath.basename (Fpath.parent dir)) input )
  |> koji_pipeline''

(* let kojitag = "pb-edvint-toolstack" in

   (* TODO: determine automatically or use cmd param *)
   Current.all
     [ dune_pipeline
     ; (let kojiurl = kojienv_apush ~code_repo ~spec_repo in
        koji_build ~kojitag kojiurl
        (* TODO: deploy/run tests *)
        |> Current.ignore_value ) ]*)

let v ~repodirs () = repo_pipeline repodirs
