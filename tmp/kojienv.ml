(* open Koji *)

let string_hash s = s |> Sha256.string |> Sha256.to_hex

module BuildRequires = struct
  include Set.Make (String)

  let digest t = t |> elements |> String.concat "\x00" |> string_hash

  let pp = Fmt.Dump.iter iter (Fmt.any "buildrequires:") Fmt.Dump.string

  let marshal t = t |> elements |> String.concat "\n"

  let unmarshal t = t |> String.split_on_char '\n' |> of_list
end

module Paths = struct
  (** Persistent state directory, completely under this plugin's control *)
  let state_dir = Current.state_dir "kojienv"

  (** Kojienv looks up the mock config based on the realpath of the current directory.
      So we cannot use symlinks to construct a parent kojienv shared by multiple checkouts of a repo.
      We need to check out the .spec repo as a real directory underneath a "kojienv" dir.

      (or we could manage our own mock configs eventually).

      We want a separate kojienv for each koji tag + buildrequires combination,
      but we want to share kojienv when building different commits of that spec repo (or source code repo),
      as long as they have the same build requirements.

      (we could also do this per koji tag + repo name, but then we wouldn't notice when build requirements are removed)

      TODO: Doing it this way with buildrequires means that we'll need to manage cleanup on our own though.
   *)
  let for_kojitag (Kojitag kojitag) build_requires =
    Fpath.(
      state_dir
      / Fmt.str "%s-BR%s" kojitag (BuildRequires.digest build_requires) )
end

module KojiEnvPath = struct
  type t = {kojitag: kojitag; path: Fpath.t}

  let v kojitag path = {kojitag; path}

  let marshal t =
    let (Kojitag tag) = t.kojitag in
    Fmt.str "%s %s" tag (Fpath.to_string t.path)

  let unmarshal str =
    match Astring.String.cut ~sep:" " str with
    | Some (tag, path) ->
        {kojitag= Kojitag tag; path= Fpath.v path}
    | None ->
        Fmt.invalid_arg "Cannot parse state: %s" str

  let digest t = Fpath.to_string t.path
  (* the kojitag is just for convenience, path uniquely identifies the kojienv *)

  let pp =
    Fmt.(
      record
        [ field "kojitag"
            (fun t ->
              let (Kojitag tag) = t.kojitag in
              tag )
            string
        ; field "path" (fun t -> t.path) Fpath.pp ] )
end

(* Don't flood the server or the local disk by creating too many kojienvs at a time. *)
let pool = Current.Pool.create ~label:"kojienv new" 8

module Plugin = struct
  (** no configuration needed *)
  type t = No_context

  let id = "kojienv"

  module Key = struct
    type t = kojitag * BuildRequires.t

    let digest (Kojitag tag, buildrequires) =
      Fmt.str "%s %s" tag (BuildRequires.digest buildrequires)

    let pp ppf (Kojitag tag, buildrequires) =
      Fmt.pf ppf "%s, %a" tag BuildRequires.pp buildrequires
  end

  module Value = KojiEnvPath

  let auto_cancel = false

  let pp = Key.pp

  let kojienv_at ~job path =
    let open Lwt.Syntax in
    let home = Sys.getenv "HOME" in
    let dir = Filename.concat home ".kojienv/links" in
    dir |> Lwt_unix.files_of_directory
    |> Lwt_stream.find_s
       @@ fun link ->
       let link = Filename.concat dir link in
       Current.Job.log job "Checking %s" link ;
       Lwt.catch
         (fun () ->
           let+ target = Lwt_unix.readlink link in
           Current.Job.log job "Checking %s with %s" link target ;
           Fpath.(equal path @@ v target) )
         (function
           | Unix.Unix_error ((ENOENT | EINVAL), _, _) ->
               Lwt.return_false
           | e ->
               Lwt.reraise e )

  let build No_context job ((Kojitag kojitag as tag), build_requires) =
    let cwd = Paths.for_kojitag tag build_requires in
    let run cmd =
      Current.Job.log job "Executing in %a" Fpath.pp cwd ;
      Current.Process.exec ~cwd ~cancellable:true ~job ("", Array.of_list cmd)
    in
    let open Lwt.Syntax in
    let* () = Current.Job.start_with ~pool ~level:Current.Level.Average job in
    let* kojienv_at = kojienv_at ~job cwd in
    let action = if Option.is_none kojienv_at then "new" else "update" in
    let open Lwt_result.Syntax in
    let* () =
      if Option.is_none kojienv_at then
        Current.Process.exec ~cancellable:true ~job
          ("", Array.of_list ["mkdir"; "-p"; Fpath.to_string cwd])
      else Lwt_result.return ()
    in
    let* () = run ["kojienv"; action; kojitag] in
    (* we've split installdeps into determining the buildrequires and installing the buildrequires,
       so that we can correctly cache kojienvs.
       Don't use installdeps here as it might then install something else than our "input".
    *)
    let+ () =
      run
        ( ["kojienv"; "mockop"; kojitag; "--install"]
        @ BuildRequires.elements build_requires )
    in
    (* TODO: on failure we need to kojienv clean? *)
    KojiEnvPath.v tag cwd
end

module Cache = Current_cache.Make (Plugin)

(** We can periodically check for updates from inherited tags *)
let default_schedule =
  Current_cache.Schedule.v ~valid_for:(Duration.of_min 30) ()

(** These commands require a Kerberos ticket nowadays,
    so take that as a dependency input *)

(** [cached_or_make ?schedule kojitag buildrequires] returns the path for a kojienv built from [kojitag],

    containing [buildrequires] packages. *)
let cached_or_make ?(schedule = default_schedule) kojitag buildrequires =
  let open Current.Syntax in
  Current.component "kojienv new/update"
  |> let> kojitag = kojitag and> buildrequires = buildrequires in
     Cache.get ~schedule Plugin.No_context (kojitag, buildrequires)

let rare = Current_cache.Schedule.v ~valid_for:(Duration.of_day 20) ()

(** [for_srpm_build ?schedule kojitag] is a kojienv where we can build an SRPM.

   Building an SRPM doesn't require an up-to-date kojienv with BuildRequires, just basic tools.
   Also determining what the buildrequires are can be done in this minimal environment.

   (we could do that by calling rpmspec outside a chroot, but that'll be the wrong version,
    and may evaluate the wrong macros, which in turn would result in the wrong buildrequires.
    E.g. we need to be careful to pick XS8 build requires for XS8, and XS9 ones for XS9 based builds.
   )
*)
let for_srpm_build ?(schedule = rare) kojitag =
  cached_or_make ~schedule kojitag
    (BuildRequires.singleton "rpm-build" |> Current.return)

let mockop ?(chain=false) env_path ~job ~phase fmt =
  let open KojiEnvPath in
  let resultdir = Printf.sprintf "SRPM/build%s" phase in
  let (Kojitag kojitag) = env_path.kojitag in
  let f args =
    let open Lwt_result.Syntax in
    let cmd =
      (* FIXME: stderr logging should really work in check_output instead! *)
      Printf.sprintf
        "kojienv mockop %S %s=%s --no-clean --no-cleanup-after -v %s"
        kojitag (if chain then "--localrepo" else "--resultdir") resultdir args
    in
    let* () =
      Current.Process.exec ~cancellable:true ~job ~cwd:env_path.path
        ("", Array.of_list ["mkdir"; "-p"; resultdir])
    in
    Current.Job.log job "Executing in %a: %s" Fpath.pp env_path.path cmd ;
    let+ output =
      Current.Process.check_output ~cancellable:true ~job ~cwd:env_path.path
        ("", [|"bash"; "-c"; cmd (* "2>&1"*)|])
    in
    (output, Fpath.(env_path.path // v resultdir))
  in
  Format.kasprintf f fmt

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

(* TODO: should really hash just xapi.spec here *)
let build_requires ~job input =
  let open Lwt_result.Syntax in
  let* specfile = find_suffix input.KojiEnvPath.path ".spec" in
  let rel_specfile =
    Filename.concat (Fpath.basename input.KojiEnvPath.path) specfile
  in
  (* we could filter in OCaml instead *)
  let+ output, _ =
    mockop ~job ~phase:"buildrequires" input
      "--quiet --chroot 'rpmspec --query --srpm --requires /local/%S 2>&1| \
       grep -v warning:'"
      rel_specfile
  in
  let br =
    output |> String.trim |> String.split_on_char '\n' |> BuildRequires.of_list
  in
  Current.Job.log job "BuildRequires: %a" BuildRequires.pp br ;
  br

let srpm ~job input =
  let open KojiEnvPath in
  let open Lwt_result.Syntax in
  let* specfile = find_suffix input.path ".spec" in
  let cwd = input.path in
  let* () =
    Current.Process.exec ~cancellable:true ~job ~cwd
      ("", [|"xenpkg"; "makesources"|])
  in
  let* _, resultdir =
    mockop ~job ~phase:"srpm" input "--buildsrpm --spec %S --sources SRPM"
      specfile
  in
  let+ name = find_suffix resultdir ".src.rpm" in
  Fpath.(resultdir / name)

let rpmbuild ~job input srpm =
  let open Lwt_result.Syntax in
  let cwd = input.KojiEnvPath.path in
  let base = Filename.basename srpm in
  let* () =
    Current.Process.exec ~cwd ~cancellable:true ~job ("", [|"cp"; srpm; base|])
  in
  let* _, resultdir =
    mockop ~job ~phase:"rpmbuild" input
      "--config-opts=rpmbuild_networking=False --rebuild %S" base
  in
  let+ name = find_suffix resultdir ".rpm" in
  Fpath.(resultdir / name)

let chain ~job input srpms =
  let open Lwt_result.Syntax in
  let cwd = input.KojiEnvPath.path in
  let bases = List.map Filename.basename srpms in
  let* () =
    Current.Process.exec ~cwd ~cancellable:true ~job
      ("", ["cp"; "-t"; "."] @ srpms |> Array.of_list)
  in
  let* _, resultdir =
    mockop ~chain:true ~job ~phase:"rpmbuild" input
      "--config-opts=rpmbuild_networking=False --chain %s"
      (List.map Filename.quote bases |> String.concat " ")
  in
  let+ names =
    Current.Process.check_output ~cwd:Fpath.(resultdir / "results") ~job ~cancellable:true ("", [|"find"; "."; "-name"; "*.rpm"; "-a"; "-not"; "-name"; "*.src.rpm"|])
  in
  names |> String.trim |> String.split_on_char '\n' |> List.map (fun s -> Fpath.(resultdir / "results" //  v s))
