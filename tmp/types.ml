module type Input = sig
  include Current_cache.S.WITH_DIGEST


  val pp : t Fmt.t
end

module PairInput (A : Input) (B : Input) = struct
  type t = A.t * B.t

  let digest (a, b) = String.concat " " [A.digest a; B.digest b]

  let pp = Fmt.Dump.pair A.pp B.pp

end

module type Output = Current_cache.S.WITH_MARSHAL

module PairOutput (A : Output) (B : Output) = struct
  type t = A.t * B.t

  let marshal (a, b) = Printf.sprintf "%S %S" (A.marshal a) (B.marshal b)

  let unmarshal s =
    Scanf.sscanf s "%S %S" (fun a b -> (A.unmarshal a, B.unmarshal b))
end

module type InOut = sig
  include Input

  include Output with type t := t
  include Map.OrderedType with type t := t
end

module Pair (A : InOut) (B : InOut) = struct
  include PairInput (A) (B)
  include PairOutput (A) (B)
  let compare (a1, b1) (a2, b2) =
    match A.compare a1 a2 with 0 -> B.compare b1 b2 | n -> n
end

module Commit = Current_git.Commit
module Tree = Current_git.Tree

type spec_tree = Spec of Tree.t

type code_tree = Code of Tree.t

module HasValidKerberosTicket = Current.Unit

type build_requires = BuildRequires of Astring.String.set

module type SetLike = sig
  type t

  val to_set : t -> Astring.String.set

  val of_set : Astring.String.set -> t
end

module StringSet (T : SetLike) = struct
  type t = T.t

  let compare a b = Astring.String.Set.compare (T.to_set a) (T.to_set b)

  let digest t =
    t |> T.to_set |> Astring.String.Set.elements |> String.concat "\x00"
    |> Sha256.string |> Sha256.to_hex

  let marshal t =
    t |> T.to_set |> Astring.String.Set.elements |> String.concat "\n"

  let unmarshal t =
    t |> String.split_on_char '\n' |> Astring.String.Set.of_list |> T.of_set

  let pp =
    Fmt.using T.to_set (Astring.String.Set.pp ~sep:Fmt.sp Fmt.Dump.string)
end

module type StringLike = sig
  type t

  val to_string : t -> string

  val of_string : string -> t
end

module MakeString (T : StringLike) = struct
  type t = T.t

  let compare a b = String.compare (T.to_string a) (T.to_string b)

  let digest t = t |> T.to_string |> Sha256.string |> Sha256.to_hex

  let marshal t = t |> T.to_string

  let unmarshal t = t |> T.of_string

  let pp = Fmt.using T.to_string Fmt.string
end

module BuildRequires = StringSet (struct
  type t = build_requires

  let to_set (BuildRequires set) = set

  let of_set set = BuildRequires set
end)

type rpms = RPMs of Astring.String.set

module RPMs = StringSet (struct
  type t = rpms

  let to_set (RPMs set) = set

  let of_set set = RPMs set
end)

type srpms = SRPMs of Astring.String.set

module SRPMs = StringSet (struct
  type t = srpms

  let to_set (SRPMs set) = set

  let of_set set = SRPMs set
end)

type builds = Builds of Astring.String.set

module Builds = StringSet (struct
  type t = builds

  let to_set (Builds builds) = builds

  let of_set set = Builds set
end)

type kojitag = Kojitag of string

(** A name for a koji tag containing build requires, and associated with a koji build target where builds can be stored.
    Koji tags aren't git tags: they are essentially a search query for all RPMs tagged with that particular name,
    and as such they are mutable: more RPMs can be added to a tag. Whereas git tags are immutable.
 *)
module KojitagName = MakeString (struct
  type t = kojitag

  let of_string str = Kojitag str

  let to_string (Kojitag tag) = tag
end)

type mock_chroot = MockChroot of string

module MockChroot = MakeString (struct
  type t = mock_chroot

  let of_string str = MockChroot str

  let to_string (MockChroot str) = str
end)

type srpm_url = SRPM of Uri.t

(** An SRPM URL, for use with `koji build`.
  Can be produced by the `kojiurl` script.
 *)
module SRPMUrl = MakeString (struct
  type t = srpm_url

  let of_string str = SRPM (Uri.of_string str |> Uri.canonicalize)

  let to_string (SRPM uri) = Uri.to_string uri
end)

type koji_repo = KojiRepo of Uri.t

(** A Koji repository URL uniquely identifies a set of RPMs, and is a valid Yum/DNF repository.
  However it may be garbage collected if other repositories are created on the same koji tag.
*)
module KojiRepo = MakeString (struct
  type t = koji_repo

  let of_string str = KojiRepo (Uri.of_string str |> Uri.canonicalize)

  let to_string (KojiRepo uri) = Uri.to_string uri
end)

type suiterun = SR of int

(** A XenRT suiterun id, it contains {!module:XenRTJob}s. *)
module SuiteRun = MakeString (struct
  type t = suiterun

  let of_string str = SR (int_of_string str)

  let to_string (SR sr) = string_of_int sr
end)

type xenrt_job = XenRTJob of int

module XenRTJob = MakeString (struct
  type t = xenrt_job

  let of_string str = XenRTJob (int_of_string str)

  let to_string (XenRTJob job) = string_of_int job
end)

type rage_report = RAGEreport of Uri.t

(** RAGEreport is a rage report Uri comparing various builds' performance. *)
module RAGEreport = MakeString (struct
  type t = rage_report

  let of_string str = RAGEreport (Uri.of_string str |> Uri.canonicalize)

  let to_string (RAGEreport uri) = Uri.to_string uri
end)


type kojitag_instance = kojitag * build_requires

type rpmbuild_env = RPMBuildEnv of (kojitag_instance * (mock_chroot * rpms))

(* only use Bos to build, but not to run command-lines.
   Bos is not Lwt-aware *)
module Cmd = Bos.Cmd

let root = Fpath.v "/"

let run_common ~fn ?(cwd = root) ~job cmd =
  let pp_cmd pf cmd =
    Fmt.pf pf "cd %a && %a" Fpath.pp cwd Current.Process.pp_cmd cmd
  in
  (* first argument is empty if we want the executable to be searched in PATH *)
  fn ?cwd:(Some cwd) ?stdin:None ?pp_cmd:(Some pp_cmd) ?pp_error_command:None
    ~cancellable:true ~job
    ("", cmd |> Cmd.to_list |> Array.of_list)

let run = run_common ~fn:Current.Process.exec

let check_output = run_common ~fn:Current.Process.check_output

let rec commands ?cwd ~job = function
  | [] ->
      Lwt_result.return ()
  | cmd :: tl ->
      let open Lwt_result.Syntax in
      let* () = run ?cwd ~job cmd in
      (commands [@tailcall]) ?cwd ~job tl

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"

  let subsystem = "pipeline_steps"

  let run_time =
    let help = "Time to execute step" in
    Summary.v_label ~help ~label_name:"name" ~namespace ~subsystem "run_time"
end

(* these follow the definitions in {!type:Current.Level.t} *)

let local_read = Current.Level.Harmless

let remote_query = Current.Level.Mostly_harmless

let build = Current.Level.Average

let notify = Current.Level.Above_average

let deploy = Current.Level.Dangerous

type 'a value = (module Output with type t = 'a)

let make_step (type key value) (module K : Input with type t = key)
    (module V : InOut with type t = value) ?schedule ~id ~level build =
  (* it is used as a log filename, so cannot contain / *)
  if String.contains id '/' then invalid_arg "ID cannot contain /";
  let module M = struct
    type t = unit

    let id = id

    module Key = K
    module Value = V

    let build () job key =
      let open Lwt.Syntax in
      let* () = Current.Job.start ~level job in
      let t0 = Mtime_clock.counter () in
      let+ result = build job key in
      let dt = Mtime_clock.count t0 in
      Prometheus.Summary.observe (Metrics.run_time id)
        (Mtime.Span.to_float_ns dt *. 1e-9) ;
      Current.Job.log job "Finished in %a" Mtime.Span.pp dt ;
      Result.iter
        (fun ok -> Current.Job.log job "Caching successful output: %a" V.pp ok)
        result ;
      result

    let pp = K.pp

    let auto_cancel = false
  end in
  let module C = Current_cache.Make (M) in
  fun arg ->
    let open Current.Syntax in
    Current.component "%s" id
    |> let> arg = arg in
       C.get ?schedule () arg

(* TODO: invalidate on use if it is not present, but TBD how to determine that.
   Perhaps the Output module should have a way to check whether it is valid, and then we could use that in the input too,
   i.e. part of InOut
*)

let cmd_exn str = Cmd.of_string str |> Result.get_ok

let check_kerberos ~duration =
  let schedule = Current_cache.Schedule.v ~valid_for:duration ()
  and id = "krenew" in
  make_step ~schedule
    (module Current.Unit)
    (module HasValidKerberosTicket)
    ~id ~level:remote_query
  @@ fun job () ->
  commands ~job
    [ cmd_exn "klist -fe"
    ; cmd_exn
        (Printf.sprintf "env KRB5_TRACE=/dev/stderr krenew -H %u -v"
           (Duration.to_min duration * 2) )
    ; cmd_exn "koji moshimoshi"
    ; cmd_exn "klist -fe" ]
