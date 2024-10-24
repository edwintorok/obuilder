open Types
open Lwt_result.Syntax

let first str =
  match Astring.String.cuts ~empty:false ~sep:" " str with
  | [] ->
      invalid_arg "Empty line"
  | hd :: _ ->
      hd

module KojitagBuilds = Pair (KojitagName) (Builds)

let kojitag_remote_builds =
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_min 10) () in
  make_step
    (module KojitagName)
    (module KojitagBuilds)
    ~id:"koji list-tagged" ~schedule ~level:remote_query
  @@ fun job (Kojitag tag as kojitag) ->
  let+ output =
    check_output ~job
      Cmd.(v "koji" % "-q" % "list-tagged" % "--inherit" % "--latest" % tag)
  in
  let set =
    output |> String.trim |> String.split_on_char '\n'
    |> Astring.String.Set.of_list
    |> Astring.String.Set.map first
  in
  (kojitag, Builds set)

let kojienv_list ~job (Kojitag tag) =
  let+ lines = check_output ~job Cmd.(v "kojienv" % "list") in
  lines |> String.split_on_char '\n'
  |> ListLabels.fold_left ~init:("", Fpath.Map.empty)
       ~f:(fun (target, acc) line ->
         let prefix = "Koji target " in
         let prefix_len = String.length prefix in
         Current.Job.log job "current target: %S, input line: %S" target line ;
         if String.starts_with ~prefix line then
           ( String.sub line prefix_len (String.length line - prefix_len - 1)
             |> String.trim
           , acc )
         else if target = tag then
           match Astring.String.cut ~sep:":" line with
           | Some (chroot, path) ->
               let path = path |> String.trim |> Fpath.v
               and chroot = MockChroot (String.trim chroot) in
               (target, Fpath.Map.add path chroot acc)
           | None ->
               Fmt.invalid_arg "Cannot parse line: %S" line
         else (target, acc) )
  |> snd

let mock_chroot_of_kojitag ~job kojitag path =
  let+ kojienvs = kojienv_list ~job kojitag in
  Current.Job.log job "Kojienvs at %a: %a" KojitagName.pp kojitag
    (Fpath.Map.pp (Fmt.pair Fpath.pp MockChroot.pp))
    kojienvs ;
  let result = Fpath.Map.find path kojienvs in
  let (Kojitag tag) = kojitag in
  Current.Job.log job "Kojienv lookup for %a: %b" Fpath.pp path
    (Option.is_some result) ;
  result

module KojitagBuildsBuildRequires = Pair (KojitagBuilds) (BuildRequires)

module Paths = struct
  let kojienvs = Current.state_dir "kojienvs"

  let kojienv_for (Kojitag tag) build_requires =
    Fpath.(kojienvs / tag / BuildRequires.digest build_requires)
end

module KojitagMap = Map.Make (KojitagName)

(* TODO: generalize to lock maps for others *)

let kojitag_lock =
  let locks = ref KojitagMap.empty in
  fun kojitag ->
    let old_locks = !locks in
    match KojitagMap.find_opt kojitag old_locks with
    | None ->
        let lock = MeasuredRWLock.create "kojitag" in
        locks := KojitagMap.add kojitag lock old_locks ;
        lock
    | Some m ->
        m

module WorktreeMap = Map.Make (Fpath)

let worktree_lock =
  let locks = ref WorktreeMap.empty in
  fun kojitag ->
    let old_locks = !locks in
    match WorktreeMap.find_opt kojitag old_locks with
    | None ->
        let lock = MeasuredRWLock.create "worktree" in
        locks := WorktreeMap.add kojitag lock old_locks ;
        lock
    | Some m ->
        m

let with_kojitag_write kojitag fn =
  MeasuredRWLock.with_write (kojitag_lock kojitag) fn

let with_kojitag_read kojitag fn =
  (* Unfortunately mock locks the build root, even for readonly ops, so we have to use the writelock here too,
     otherwise we get an error from mock:"ERROR: Build root is locked by another process"
  *)
  MeasuredRWLock.with_write (kojitag_lock kojitag) fn

module KojitagInstance = Pair (KojitagName) (BuildRequires)

(* TODO: check that it exists, if not invalidate original! we could've cleaned it up... *)
let mockop ((Kojitag tag as kojitag), build_requires) ~job args =
  with_kojitag_read kojitag
  @@ fun () ->
  let cwd = Paths.kojienv_for kojitag build_requires in
  check_output ~cwd ~job Cmd.(v "kojienv" % "mockop" % tag % "--no-clean" % "--no-cleanup-after" %% args)

let list_installed kojitag =
  mockop kojitag Cmd.(v "--pm-cmd" % "list" % "--installed")

let kojitag_installed ~cwd ~job kojitag_instance =
  let+ installed = list_installed ~job kojitag_instance in
  (* yum list doesn't print the name and version in the same format as rpm -qa *)
  installed |> String.split_on_char '\n'
  |> List.filter_map
     @@ fun line ->
     match Astring.String.cuts ~sep:" " ~empty:false line with
     | [pkg; ver; _] -> (
       match Astring.String.cut ~sep:"." pkg with
       | Some (name, ext) ->
           Some (Printf.sprintf "%s-%s.%s.rpm" name ver ext)
       | None ->
           Fmt.invalid_arg "Cannot parse package name from package manager: %s"
             pkg )
     | _ ->
         None

let set s = s |> Astring.String.Set.elements |> Cmd.of_list

let kojitag_new_or_update kojitag_builds build_requires =
  let module O = Pair (KojitagInstance) (Pair (MockChroot) (RPMs)) in
  Current.pair kojitag_builds build_requires
  |> ( make_step
         (module KojitagBuildsBuildRequires)
         (module O)
         ~id:"kojienv new&update" ~level:remote_query
     @@ fun job (((Kojitag tag as kojitag), _builds), build_requires) ->
     (* We only use the builds for caching, and otherwise ignore it *)
     let cwd = Paths.kojienv_for kojitag build_requires in
     let* () = run ~job Cmd.(v "mkdir" % "-p" % p cwd) in
     let* mock_chroot = mock_chroot_of_kojitag ~job kojitag cwd in
     let* mock_chroot =
       with_kojitag_write kojitag
       @@ fun () ->
       match mock_chroot with
       | Some chroot ->
           let+ () = run ~job ~cwd Cmd.(v "kojienv" % "update" % tag) in
           chroot
       | None -> (
           let* () = run ~job ~cwd Cmd.(v "kojienv" % "new" % tag) in
           let+ mock_chroot = mock_chroot_of_kojitag ~job kojitag cwd in
           match mock_chroot with
           | None ->
               Fmt.invalid_arg
                 "Newly created mock chroot not found for %s at %a" tag Fpath.pp
                 cwd
           | Some r ->
               r )
     in
     let kojitag_instance = (kojitag, build_requires) in
     let (BuildRequires pkgs) = build_requires in
     let* install_out =
       mockop ~job kojitag_instance Cmd.(v "-v" % "--install" %% set pkgs)
     in
     Current.Job.log job "Install packages output: %s" install_out ;
     let+ pkgs = kojitag_installed ~cwd ~job kojitag_instance in
     (kojitag_instance, (mock_chroot, RPMs (Astring.String.Set.of_list pkgs)))
     )
  |> Current.map (fun x -> RPMBuildEnv x)

let mockop_chroot kojitag cmd =
  (* TODO: need to lock the chroot! the same way we lock kojienv *)
  mockop kojitag Cmd.(v "--no-clean" %  "--no-cleanup-after" % (*"--quiet" % *) "--chroot" % Cmd.to_string cmd)

let filter_builds pkg (kojitag, Builds builds) =
  ( kojitag
  , Builds
      (builds |> Astring.String.Set.filter @@ String.starts_with ~prefix:pkg) )

let srpm_kojienv kojitag_remote_builds =
  let builds = kojitag_remote_builds |> Current.map (filter_builds "rpm-build")
  and build_requires =
    BuildRequires (Astring.String.Set.singleton "@build") |> Current.return
  in
  kojitag_new_or_update builds build_requires

let with_worktree' ?pool ~job commit path
    (fn : Fpath.t -> 'a Current.or_error Lwt.t) : 'a Current.or_error Lwt.t =
  let open Current_git in
  let repo = Commit.repo commit and id = Commit.id commit in
  let short_hash = Astring.String.with_range ~len:8 (Commit_id.hash id) in
  Current.Job.log job "@[<v2>Checking out commit %s. To reproduce:@,%a@]"
    short_hash Commit_id.pp_user_clone id ;
  (* TODO: worktree lock *)
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
      let* () =
        MeasuredRWLock.with_write (worktree_lock path)
        @@ fun () ->
        let* () = run ~job Cmd.(v "rm" % "-rf" % p path) in
        (* TODO: for now *)
        run ~job
          Cmd.(
            v "git" % "-C" % p repo % "worktree" % "add" % "--force"
            % "--detach" % p path % Commit_id.hash id )
      in
      let* () = Current.Switch.turn_off switch |> Lwt_result.ok in
      fn path )
    (fun () -> Current.Switch.turn_off switch)

let find_suffixes cwd suffix =
  Lwt_unix.files_of_directory (Fpath.to_string cwd)
  |> Lwt_stream.filter (String.ends_with ~suffix)
  |> Lwt_stream.to_list
  |> Lwt.map
     @@ function
     | [] ->
         Fmt.error_msg "No file with suffix %S was found in %a" suffix Fpath.pp
           cwd
     | files ->
         Ok files

let find_suffix cwd suffix =
  let open Lwt_result.Syntax in
  let+ files = find_suffixes cwd suffix in
  List.hd files

type kind = Requires | Provides

let rpmspec kind env spec_tree =
  let module K = Pair (Pair (KojitagInstance) (Pair (MockChroot) (RPMs))) (Tree)
  in
  let env = env |> Current.map @@ fun (RPMBuildEnv env) -> env
  and spec_tree = spec_tree |> Current.map @@ fun (Spec spec) -> spec in
  let cmd =
    match kind with
    | Requires ->
        Cmd.(v "--srpm" % "--requires")
    | Provides ->
        Cmd.v "--provides"
  in
  Current.pair env spec_tree
  |> make_step
       (module K)
       (module BuildRequires)
       ~id:("rpmspec " ^ Cmd.to_string cmd)
       ~level:local_read
     @@ fun job (((kojitag, build_requires), _mock), spec_tree) ->
     let commit = Tree.commit spec_tree
     and cwd = Paths.kojienv_for kojitag build_requires
     and rel_path = Tree.tree_hash spec_tree |> Fpath.v in
     let path = Fpath.(cwd // rel_path) in
     with_worktree' ~job commit path
     @@ fun path ->
     let* file_name = find_suffix path ".spec" in
     let rel_path = Fpath.(rel_path / file_name) in
     let+ lines =
       mockop_chroot ~job (kojitag, build_requires)
         Cmd.(v "rpmspec" % "--query" %% cmd % p Fpath.(v "/local" // rel_path))
     in
     BuildRequires
       ( lines |> String.trim |> String.split_on_char '\n'
       |> List.filter_map (fun line ->
              let line = String.trim line in
              if String.equal line "" then None else Some line )
       |> Astring.String.Set.of_list )

let pkg_name str =
  match Astring.String.cut ~sep:"=" str with
  | None ->
      Fmt.invalid_arg "Cannot parse line %S" str
  | Some (pkg, ver) ->
      String.trim pkg

let build_requires = rpmspec Requires

let provides srpm_env spec =
  rpmspec Provides srpm_env spec
  |> Current.map (fun (BuildRequires br) ->
         BuildRequires (Astring.String.Set.map pkg_name br) )

(* TODO: specsync
   TODO: write as .tar not .tar.gz, or override compression level for gz?
*)

let define key value = Cmd.(v "--define" % Printf.sprintf {|%s %s|} key value)

(* If we had a newer version of RPM we could've used zstd level 0 or 1
   that would've still compressed it, but very quickly.
   The version of RPM we have only supports slow compression options
   (gzip, xz, lzma)
   See rpmio.h for the meaning of 'w.ufdio'*)
let uncompressed = "w.ufdio"

let fast_source = define "_source_payload" uncompressed

let fastbuild =
  Cmd.(
    (* faster to include the full binary than to extract the debug symbols separately, which is sequential and slow *)
    define "debug_package" "%{nil}"
    %% define "_binary_payload" uncompressed
    %% fast_source )


let no_network = Cmd.(v "--config-opts=rpmbuild_networking=False")
let srpm env spec_tree code_tree =
  (* TODO:do we need --no-clean in more places? I get some package installs during the srpm run here *)
  let module K =
    Pair
      (Pair (KojitagInstance) (Pair (MockChroot) (RPMs))) (Pair (Tree) (Tree))
  in
  let env = env |> Current.map @@ fun (RPMBuildEnv env) -> env
  and spec_tree = spec_tree |> Current.map @@ fun (Spec spec) -> spec
  and code_tree = code_tree |> Current.map @@ fun (Code spec) -> spec in
  Current.(pair env (pair spec_tree code_tree))
  |> make_step (module K) (module SRPMs) ~id:"srpm" ~level:build
     (* cheaper than a binary build, but still potentially handles a lot of files *)
     @@ fun job (((kojitag, build_requires), _mock), (spec_tree, code_tree)) ->
     let commit = Tree.commit spec_tree
     and commit_scm = Tree.commit code_tree
     and cwd = Paths.kojienv_for kojitag build_requires
     and rel_path = Tree.tree_hash spec_tree |> Fpath.v in
     let path = Fpath.(cwd // rel_path) in
     let* file_name = find_suffix path ".spec" in
     with_worktree' ~job commit path
     @@ fun path ->
     with_worktree' ~job commit_scm Fpath.(path / "scm")
     @@ fun _scm_path ->
     let  spec = Fpath.(rel_path / file_name) in
     let  srpm = Fpath.(rel_path / "SRPM") in
     let* ()=
       run ~cwd:path ~job Cmd.(v "xenpkg" % "makesources")
     in
     let* lines =
       mockop ~job (kojitag, build_requires)
         Cmd.(
           v "--buildsrpm" % "--spec" % p spec % "--sources" % p srpm % "-v"
           %% no_network
           % "--resultdir" % p Fpath.(srpm / "buildsrpm")
           %% fast_source )
     in
     Current.Job.log job "SRPM build output: %s" lines ;
     let root = Fpath.(path / "SRPM" / "buildsrpm") in
     let+ path = find_suffix root ".src.rpm" in
     SRPMs (Fpath.(root / path |> to_string) |> Astring.String.Set.singleton)

let cache_mount cwd =
  (* TODO: requires ~/.config/mock.cfg, write out default one? *)
  let home = Sys.getenv "HOME" in
  let mounts =
    [ (Fpath.to_string cwd, "/local")
    ; (Filename.concat home ".cache/dune", "/builddir/.cache/dune") ]
    |> List.map (fun (k, v) ->
           Printf.sprintf "(%s, %s)" (Filename.quote k) (Filename.quote v) )
    |> String.concat ", " |> Printf.sprintf "[%s]"
  in
  Cmd.(v ("--plugin-option=bind_mount:dirs=" ^ mounts))

let chain env srpms =
  let module K =
    Pair (Pair (KojitagInstance) (Pair (MockChroot) (RPMs))) (SRPMs)
  in
  let env = env |> Current.map @@ fun (RPMBuildEnv env) -> env in
  Current.(pair env srpms)
  |> make_step (module K) (module RPMs) ~id:"mock --chain" ~level:build
     (* cheaper than a binary build, but still potentially handles a lot of files *)
     @@ fun job (((kojitag, build_requires), _mock), (SRPMs srpms as input)) ->
     let cwd = Paths.kojienv_for kojitag build_requires in
     let* () = run ~job Cmd.(v "cp" % "-t" % p cwd %% set srpms) in
     let localrepo = Fpath.(v "chain" / SRPMs.digest input) in
     let* () = run ~cwd ~job Cmd.(v "mkdir" % "-p" % p localrepo) in
     let* lines =
       (* TODO: enable ccache plugin?
          how to enable dune cache? need to bind mount + set env var
          do we need our own mock plugin?
       *)
       mockop ~job (kojitag, build_requires)
         Cmd.(
           v "--no-clean" % "--no-cleanup-after" % "-v"
           %% no_network
           % ("--localrepo=" ^ Fpath.to_string localrepo)
           %% cache_mount cwd %% fastbuild % "--chain"
           %% set (Astring.String.Set.map Filename.basename srpms) )
     in
     Current.Job.log job "chainbuild output: %s" lines ;
     let Kojitag tag = kojitag in
     let+ lines = check_output ~job Cmd.(v "find" % p Fpath.(cwd // localrepo) % "-name" % "*.rpm") in
     RPMs (lines |> String.trim |> String.split_on_char '\n' |> Astring.String.Set.of_list)

let koji_config =
  ( Bos.Pat.v "koji-$(TAG)-x86_64"
  , Bos.Pat.v
      {|
[koji-$(TAG)-x86_64]
name=$(TAG)-latest-x86_64
baseurl=https://koji.eng.citrite.net/kojifiles/repos/$(TAG)/latest/x86_64
enabled=1
gpgcheck=0
metadata_expire=0

[koji-$(TAG)-src]
name=$(TAG)-latest-src
baseurl=https://koji.eng.citrite.net/kojifiles/repos/$(TAG)/latest/src
enabled=1
gpgcheck=0
metadata_expire=0
|}
  )

let local_config =
  ( Bos.Pat.v "local_rpms"
  , Bos.Pat.v
      {|
[local_rpms]
name=local_rpms
baseurl=file://$(RPMS)
enabled=1
gpgcheck=0
metadata_expire=0
priority=10
|}
  )

let setup_repo (Kojitag tag) destination =
  (* There is yum-config-manager, but its setopt doesn't work on repositories with . in the name,
     but the autogenerated repoid for a remote URL will contain dots, and you cannot override this on the CLI.
     Just generate the file directly.
  *)
  let defs =
    Astring.String.Map.of_list
      [("TAG", tag); ("RPMS", Fpath.to_string destination)]
  in
  let create_config name (pat_name, pat_config) =
    let config = Bos.Pat.format defs pat_config in
    let echo = Cmd.(v "echo" % config |> to_string) in
    ( Bos.Pat.format defs pat_name
    , Cmd.(v "sh" % echo % Printf.sprintf ">/etc/yum.repos.d/%s.repo" name) )
  in
  let repo1, koji = create_config "koji" koji_config
  and repo2, local = create_config "local" local_config in
  ([repo1; repo2], [koji; local])

let ssh = Cmd.(v "ssh" % "-o" % "StrictHostKeyChecking=no" % "-l" % "root")

let install_rpms kojitag rpms host =
  let module K = PairInput (PairInput (KojitagName) (RPMs)) (Current.String) in
  Current.(pair (pair kojitag rpms) host)
  |> make_step (module K) (module RPMs) ~id:"rsync&install" ~level:deploy
     @@ fun job ((kojitag, RPMs rpms), host) ->
     let dest = Fpath.v "/var/log/rpms" in
     let repos, setup = setup_repo kojitag dest in
     let* () =
       [ [ Cmd.(
             v "rsync" % "--partial" % "--info=progress2" % "--fuzzy" % "-e"
             % Cmd.to_string ssh
             %% (rpms |> Astring.String.Set.elements |> Cmd.of_list)
             % (host ^ ":" ^ Fpath.to_string dest)
             (* avoid ENOSPC on / *) ) ]
       ; setup
       ; [ Cmd.(v "yum" % "install" % "-y" % "yum-plugin-priorities")
         ; Cmd.(
             v "yum" % "distro-sync" % "--disable-repo=*"
             %% Cmd.of_list ~slip:"--enablerepo" repos ) ] ]
       |> List.concat |> commands ~job
     in
     let+ lines = check_output ~job Cmd.(v "rpm" % "-qa") in
     RPMs (lines |> String.split_on_char '\n' |> Astring.String.Set.of_list)

let toolstack_restart host rpms =
  let module K = PairInput (RPMs) (Current.String) in
  Current.pair rpms host
  |> make_step
       (module K)
       (module Current.Unit)
       ~id:"xe-toolstack-restart" ~level:deploy
     @@ fun job (_rpms, host) ->
     (* rpms is used as cache input, but not actually used in the actual command, we just want to know whether any RPMs have changed or not *)
     run ~job Cmd.(ssh % host % "xe-toolstack-restart")

