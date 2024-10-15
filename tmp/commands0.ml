(* only use Bos to build, but not to run command-lines.
   Bos is not Lwt-aware *)
module Cmd = Bos.Cmd

module Dune = struct
   let build ?(trace=false) args =
      let errors =
         (* print errors early when watching interactively,
            but also summarize errors at the end so we can easily find them *)
         Cmd.(v "--stop-on-first-error" % "--error-reporting=twice")
      and dev =
         (* close to --release, but better suited for incremental builds *)
         Cmd.(v "--root" % "." % "--ignore-promoted-rules" % "--no-config" % "--always-show-command-line" % "--promote-install-files=false" % "--require-dune-project-file" % "--ignore-lock-dir")
      and cache =
         Cmd.(v "--cache=enabled")
      and stderr_require_empty =
         Cmd.(v "--action-stdout-on-success=swallow" % "--action-stderr-on-success=must-be-empty")
      and trace =
         Cmd.(on trace @@ v "--trace-file" % "_build/trace.json")
      in
      Cmd.(v "dune" % "build" %% errors %% stderr_require_empty %% dev %% cache %% trace %% args)

   (* TODO: wrap with opam, caches, etc: *)
end
