let () = Prometheus_unix.Logging.init ()

let program_name = Filename.basename Sys.executable_name

let main config mode repodirs =
  let repodirs = List.map Fpath.v repodirs in
  let engine = Current.Engine.create ~config (Pipeline.v ~repodirs) in
  let has_role user = function
    | `Viewer | `Monitor -> true
    | _ ->
      Option.map Current_web.User.id user = Some Simple_auth.admin
  in
  let site =
    Current_web.Site.(v ~authn:Simple_auth.auth ~refresh_pipeline:5 ~has_role)
      ~name:program_name
      (Simple_auth.route :: Current_web.routes engine)
  in
  Logs.set_level ~all:true (Some Logs.Debug) ;
  Logs.Src.list () |> List.iter (fun src ->
    if Logs.Src.name src = "cohttp.lwt.io" then
      Logs.Src.set_level src (Some Logs.Info)
  );
  Lwt_main.run
    (Lwt.choose
       [ Current.Engine.thread engine
       ; (* The main thread evaluating the pipeline. *)
         Current_web.run ~mode site (* Optional: provides a web UI *) ] )

open Cmdliner

(* An example command-line argument: the repository to monitor *)
let repodirs =
  Arg.value @@ Arg.pos_all Arg.dir []
  @@ Arg.info ~doc:"Git source directory" ~docv:"DIR" []

let cmd =
  let doc = "an OCurrent pipeline" in
  let info = Cmd.info program_name ~doc in
  Cmd.v info
    Term.(
      term_result
        (const main $ Current.Config.cmdliner $ Current_web.cmdliner $ repodirs) )

let () = Cmd.(exit @@ eval cmd)
