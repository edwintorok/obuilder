type command =
  { cwd: Fpath.t option  (** working directory of command *)
  ; args: string list
        (** command to run (looked up in [$PATH]) and its arguments. *) }

module Koji = struct
  type target = Target of string

  type environment = Environment of string
end

module Repo = struct
  type spec = Spec of Fpath.t

  type code = Code of Fpath.t

  module SpecSCM = struct
    type t = spec * code

    let scmdir = "scm"
    (* we should get this from 'xenpkg getinfo scmdir', but that can only be run inside a checkout *)

    let v (Spec spec) (Code code) =
      ( (Spec spec, Code code)
      , { cwd= None
        ; args=
            [ "ln"
            ; "-s"
            ; Fpath.to_string code
            ; Fpath.(spec / scmdir |> to_string) ] } )
  end
end

module Dune = struct
  let build args (Repo.Code code) =
    (* need to use unique dir to allow multiple separate dune commands to run.
       This can be useful if we want to gate/launch more commands based on the completion of some dune rules
       (e.g. @check)
    *)
    let unique_dir =
      Digest.string (String.concat "\x00" args) |> Digest.to_hex
    in
    (* must be absolute path if subdir 
    let unique_dir = Fpath.(code / "_build" / unique_dir |> to_string) in*)
    [ {cwd= None; args= ["mkdir"; "-p"; unique_dir]}
    ; { cwd= Some code
      ; args=
          "opam" :: "exec" :: "--switch" :: "4.14.2" :: "--" :: "dune"
          :: "build" :: "--stop-on-first-error" :: "--error-reporting=twice"
          :: "--root" :: "." :: "--ignore-promoted-rules" :: "--no-config"
          :: "--always-show-command-line" :: "--promote-install-files=false"
          :: "--require-dune-project-file" :: "--ignore-lock-dir"
          :: "--cache=enabled"
(*          :: ("--build-dir=" ^ unique_dir)*)
          :: args } ]

  let fmt =
    (* for some reason autodetection of git diff doesn't work when called from here *)
    build ["@fmt"; "--diff-command"; "diff"]

  let lint = build ["@lint"]

  let check = build ["@check"]

  let doc = build ["@doc"]

  let default =  build ["@@default"]

  let install = build ["@install"]

  let runtest =
    (* TODO: ulimit *)
    build ["@runtest"]
end

type package = Package of string

module Kojienv = struct
  let new_ (Koji.Target tag) = {cwd= None; args= ["kojienv"; "new"; tag]}

  let clean (Koji.Environment env) = {cwd= None; args= ["kojienv"; "clean"; env]}

  let kojienv ~spec:((Spec spec_repo, _) : Repo.SpecSCM.t) args =
    {cwd= Some spec_repo; args= "kojienv" :: args}

  let kojienv_tag cmd args (Koji.Target tag) = kojienv (cmd :: tag :: args)

  let configure = kojienv_tag "configure" []

  let run tag cmd = kojienv_tag "run" cmd tag

  let scratch ?(clean = false) ?(remote = false) =
    let cmd =
      if remote then "kscratch" else if clean then "cscratch" else "scratch"
    in
    kojienv_tag cmd []

  let apush = kojienv ["apush"]

  let kojienv_tag' cmd args (Koji.Target tag) ~spec_repo:(Repo.Spec spec) =
    {cwd= Some spec; args= "kojienv" :: cmd :: tag :: args}

  let build = kojienv_tag' "build" []

  let installdeps ~specfile = kojienv_tag' "installdeps" [specfile]

  let arg_package (Package p) = p

  let install ~packages = kojienv_tag' "install" (List.map arg_package packages)

  let update ~packages = kojienv_tag' "update" (List.map arg_package packages)

  let mockop ops = kojienv_tag' "mockop" ops
end

let flag cond name = if cond then ["--" ^ name] else []

module Xenpkg = struct
  let clone ~target (Package package) =
    {cwd= Some target; args= ["xenpkg"; "clone"; package]}

  let xenpkg' ~spec_repo:(Repo.Spec spec) args =
    {cwd= Some spec; args= "xenpkg" :: args}

  let xenpkg ~spec:(Repo.Spec spec, _) args =
    {cwd= Some spec; args= "xenpkg" :: args}

  let sync = xenpkg' ["sync"]

  let specsync = xenpkg ["specsync"]

  let makesources ?(cimode = false) () =
    xenpkg ("makesources" :: flag cimode "cimode")

  let addchecksum paths =
    xenpkg' ("addchecksum" :: List.map Fpath.to_string paths)

  type info = Srpmdir | Scmdir | Patchdir

  let getinfo info =
    xenpkg'
      [ "getinfo"
      ; ( match info with
        | Srpmdir ->
            "srpmdir"
        | Scmdir ->
            "scmdir"
        | Patchdir ->
            "patchdir" ) ]

  let status = xenpkg' ["status"]

  let release = xenpkg ["release"]

  let pqsync = xenpkg ["pqsync"]
end

module Kapi = struct
  type suffix =
    | Suffix of string  (** koji tag suffix, like pb-<youruser>-<suffix> *)

  type username = Username of string  (** can get it from 'getinfo whoami' *)

  let tag_of_suffix (Username username) (Suffix suffix) =
    Koji.Target (Printf.sprintf "pb-%s-%s" username suffix)

  type parent = Parent of string

  type suite = Suite of string

  type build = Build of string

  let kapi cmd args =
    (* kapi has some unusual conventions where commands and arguments are all '--' *)
    {cwd= None; args= "kapi" :: ("--" ^ cmd) :: List.concat args}

  let whoami = kapi "whoami" []

  let list_tags = kapi "list-tags" []

  let arg_suffix (Suffix suffix) = ["--suffix"; suffix]

  let arg_parent (Parent p) = ["--parent"; p]

  let create_tag suffix ~parent =
    kapi "create-tag" [arg_suffix suffix; arg_parent parent]

  let delete_tag suffix = kapi "delete-tag" [arg_suffix suffix]

  let regen_repo suffix = kapi "regen-repo" [arg_suffix suffix]

  let add_target suffix = kapi "add-target" [arg_suffix suffix]

  let remove_target suffix = kapi "remove-target" [arg_suffix suffix]

  let clone_tag ~source:(Koji.Target tag) suffix source =
    let source =
      match source with
      | `Event id ->
          ["--event"; string_of_int id]
      | `Repoid id ->
          ["--repoid"; string_of_int id]
    in
    kapi "clone-tag" [["--source"; tag]; arg_suffix suffix; source]

  let permit_package suffix ~package:(Package p) =
    kapi "permit-package" [arg_suffix suffix; ["--package"; p]]

  let arg_build (Build b) = ["--build"; b]

  let view_karma build = kapi "view-karma" [arg_build build]

  let karma_reset builds = kapi "view-karma" (List.map arg_build builds)

  type what =
    | Tag of Koji.target
    | Url of {url: Uri.t; branch: string; version: string}

  let request_suite (Suite testsuite) ?failnotify what =
    kapi "request-suite"
    @@ ["--suite"; testsuite]
       ::
       (let args =
          match what with
          | Tag (Koji.Target tag) ->
              ["--tag"; tag]
          | Url {url; branch; version} ->
              [ "--url"
              ; Uri.to_string url
              ; "--branch"
              ; branch
              ; "--version"
              ; version ]
        in
        match failnotify with
        | None ->
            [args]
        | Some failnotify ->
            [["--failnotify"; failnotify]; args] )
end
