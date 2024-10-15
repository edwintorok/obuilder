(**
  Typed description of factory tools [xenpkg],[kojienv],[koji],[kapi],[kopi].
  Only the commands that are usable in batch mode are defined.

  Includes prerequisites for commands (e.g. which directory they must be run from, what other directories must be present, etc).
*)


type command =
  { cwd : Fpath.t option (** working directory of command *)
  ; args : string list (** command to run (looked up in [$PATH]) and its arguments. *)
  }

module Koji_ : sig
  type target = Target of string (** a Koji Tag (target) *)
  type environment = Environment of string (** a kojienv mock environment *)
end

module Repo: sig
  type spec = Spec of Fpath.t
  type code = Code of Fpath.t
  module SpecSCM: sig
    type t = private spec * code
    (** a spec repo with an 'scm' (symlink) pointing to the code repo *)

    val v: spec -> code -> t * command
  end
end

module Dune: sig
  val fmt: Repo.code -> command list
  val lint: Repo.code -> command list
  val check: Repo.code -> command list
  val doc: Repo.code -> command list
  val default: Repo.code -> command list
  val install: Repo.code -> command list
  val runtest: Repo.code -> command list
end


type package = Package of string

module Kojienv_: sig
  val new_ : Koji_.target -> command

  val clean: Koji_.environment -> command

  val configure : Koji_.target -> spec: Repo.SpecSCM.t -> command

  val run : Koji_.target -> string list -> spec:Repo.SpecSCM.t -> command

  val scratch : ?clean:bool -> ?remote:bool -> Koji_.target -> spec:Repo.SpecSCM.t -> command

  val apush : spec:Repo.SpecSCM.t -> command

  val build:  Koji_.target -> spec_repo:Repo.spec -> command

  val installdeps: specfile:string -> Koji_.target -> spec_repo:Repo.spec -> command

  val install : packages:package list -> Koji_.target -> spec_repo:Repo.spec -> command

  val update: packages:package list -> Koji_.target -> spec_repo:Repo.spec -> command

  val mockop: string list -> Koji_.target -> spec_repo:Repo.spec -> command
end

module Xenpkg : sig
  val clone : target:Fpath.t -> package -> command

  val sync: spec_repo:Repo.spec -> command

  val specsync: spec:Repo.SpecSCM.t -> command

  val makesources : ?cimode:bool -> unit -> spec:Repo.SpecSCM.t -> command

  val addchecksum: Fpath.t list -> spec_repo:Repo.spec -> command

  type info = Srpmdir | Scmdir | Patchdir
  val getinfo : info -> spec_repo:Repo.spec -> command

  val status : spec_repo:Repo.spec -> command

  val release: spec:Repo.SpecSCM.t -> command

  val pqsync: spec:Repo.SpecSCM.t -> command
end

module Kapi: sig

  type suffix = Suffix of string (** koji tag suffix, like pb-<youruser>-<suffix> *)
  type username = Username of string (** can get it from 'getinfo whoami' *)

  val tag_of_suffix: username -> suffix -> Koji_.target

  type parent = Parent of string

  type suite = Suite of string

  type build = Build of string

  val whoami : command

  val list_tags: command

  val create_tag: suffix -> parent:parent -> command

  val delete_tag: suffix -> command

  val regen_repo: suffix -> command

  val add_target: suffix -> command

  val remove_target: suffix -> command

  val clone_tag: source:Koji_.target -> suffix -> [< `Event of int | `Repoid of int] -> command

  val permit_package: suffix -> package:package -> command

  val view_karma: build -> command

  val karma_reset : build list -> command
  
  type what = Tag of Koji_.target | Url of { url: Uri.t; branch: string; version: string }
  val request_suite : suite -> ?failnotify:string -> what -> command  
end
