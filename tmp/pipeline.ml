(*
   We have a sandbox, and unsandboxed part of the pipeline.
   The unsandboxed dune one could be optional, so we don't depend on anything in the user's environment?
*)

open Types
open Current.Syntax

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
  let repo_pipe label repo =
    repo |> Local.head_commit |> tree_hash
    |> Current.collapse ~key:"repo"
         ~value:(Local.repo repo |> Fpath.to_string)
         ~input:(Current.return ~label repo)
  in
  let code =
    let path =
      code_repo |> Local.repo
      |> Fpath.relativize ~root:(spec_repo |> Local.repo |> Fpath.parent)
      |> Option.get
    in
    repo_pipe (Fpath.to_string path) code_repo
  and spec = repo_pipe (spec_repo |> Local.repo |> Fpath.basename) spec_repo in
  (Current.map (fun c -> Code c) code, Current.map (fun s -> Spec s) spec)

let config = Current.Config.v ()

(*
let () = Logs.Src.set_level pipeline (Some Logs.Debug)
*)

let prefix = "private/edvint/ci/"

let koji_pipeline_env tag =
  let kojitag = Kojitag tag in
  let has_kerberos =
    Types.check_kerberos ~duration:(Duration.of_min 5) (Current.return ())
    |> Current.map (fun _ -> ())
  in
  let kojitag = Current.return kojitag in
  let builds = Steps.kojitag_remote_builds kojitag in
  let srpm_env = Steps.srpm_kojienv (Current.gate ~on:has_kerberos builds) in
  (has_kerberos, kojitag, builds, srpm_env)

let build_requires_union (BuildRequires a) (BuildRequires b) =
  BuildRequires (Astring.String.Set.union a b)

let build_requires_union_all lst =
  List.fold_left build_requires_union (BuildRequires Astring.String.Set.empty)
    lst

let maybe_pkg_name str =
  match Astring.String.cut ~sep:" " str with
  | None -> str
  | Some (pkg, ver) ->
      String.trim pkg

(* build requires outside of the local list of packages that we are monitoring *)
let build_requires_external ~provides:(BuildRequires provides)
    (BuildRequires build_requires) =
  let build_requires_names_only = Astring.String.Set.map maybe_pkg_name build_requires in
  let required_names_only = Astring.String.Set.diff build_requires_names_only provides in
  (* we could have requirements of the form:'xapi-idl-devel >= ...', and 'openssl-devel >= ...'.
    We want to preserve the constraint on the latter, but drop the constraint on the former when eliminating local provides.
   *)
  BuildRequires (Astring.String.Set.filter (fun build_requires ->
    Astring.String.Set.mem (maybe_pkg_name build_requires) required_names_only
  ) build_requires)

let koji_pipeline_srpm (_, kojitag, builds, srpm_env) (code, spec) =
  Steps.srpm srpm_env spec code

let koji_pipeline_rpm ~deps ~provides (on, kojitag, builds, srpm_env) (spec, srpm) =
  let build_requires =
    Steps.build_requires srpm_env spec
    |> Current.pair provides
    |> Current.map
       @@ fun (provides, build_requires) ->
       build_requires_external ~provides build_requires
  in
  let on = Current.all [on; Current.ignore_value deps] in
  (* TODO:filter out locally produced deps *)
  let rpmbuild_env = build_requires |> Current.gate ~on |>Steps.kojitag_new_or_update builds in
  Steps.chain rpmbuild_env srpm
  
let koji_pipeline_rpms srpms =
  List.map Current.ignore_value srpms |> Current.all

let path_of_spec spec =
  let Spec tree = spec in
  tree |> Tree.commit |> Commit.repo |> Fpath.to_string

let analyze_spec srpm_env spec =
  let open Current.Syntax in
  let open Astring in
  let+ BuildRequires requires = Steps.build_requires srpm_env spec
  and+ spec
  and+ BuildRequires provides = Steps.provides srpm_env spec in
  let name = path_of_spec spec in
  (* RPM Provides -> depends on .src.rpm build -> depends on BuildRequires *)
  let provides_to_srpm_build = provides |> String.Set.to_seq |> Seq.map (fun p -> (maybe_pkg_name p, [name])) |> List.of_seq
  and srpm_to_build_requires = name, requires |> String.Set.elements |> List.map maybe_pkg_name in
  (name, spec), srpm_to_build_requires :: provides_to_srpm_build


let sorted_dependencies srpm_env specs =
  let open Current.Syntax in
  let+ names_and_deps =
    specs |> List.map (analyze_spec srpm_env)
    |> Current.list_seq
  in
  let names, deps = List.split names_and_deps in
  let path_to_spec = Astring.String.Map.of_list names in
  let deps = List.concat deps in
  (deps |> List.iter @@  fun deplist -> Logs.info (fun m -> m "Deps: %a" Fmt.(Dump.pair string (Dump.list string)) deplist));
  match Tsort.sort deps with
  | Tsort.ErrorCycle cycle -> Fmt.invalid_arg "Dependency cycle between .spec: %a" Fmt.(Dump.list string) cycle
  | Tsort.Sorted order ->
    let result = order |> List.filter_map (fun dep -> Astring.String.Map.find_opt dep path_to_spec ) in
    Logs.debug (fun m -> m "dependencies: %a" Fmt.(Dump.list @@ using path_of_spec string) result);
    (* TODO: a type a t with the srpm/spec that has all needed info: requires/provides, and where we can build dep graphs,
    now we've got the toposort, but still need to find the deps to build the graph *)
    result

let repo_pipeline dirs =
  let open Current_git in
  let ((on, kojitag, builds, srpm_env) as env) =
    koji_pipeline_env "pb-edvint-toolstack"
  in
  let codes_specs = dirs |> List.map input_of in
  let codes, specs = codes_specs |> List.split in
  let srpms = codes_specs |>List.map (koji_pipeline_srpm env) in

  let deps =
    sorted_dependencies srpm_env specs

  in
  
  let provides =
    specs
    |> List.map (Steps.provides srpm_env)
    |> Current.list_seq
    |> Current.map build_requires_union_all
   |>Current.gate ~on
  
  in
  List.combine specs srpms |> List.map (koji_pipeline_rpm ~deps ~provides env) |> koji_pipeline_rpms

let v ~repodirs () = repo_pipeline repodirs
