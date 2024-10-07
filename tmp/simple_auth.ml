open Lwt.Infix
module Server = Cohttp_lwt_unix.Server

(* TODO: use kerberos Negotiate/GSSAPI instead, or OAuth?
   Probably once we move to kfd and have https
 *)

let login_form csrf =
  let open Tyxml in
  [%html
    {|<form action="login" method="Post">
        <input name="state" value=|}
      csrf
      {| type="hidden"/>
        <label for="password">Password</label>
        <input type="password" id="password" name="password" required/>
        <input type="Submit" value="Sign in"/>
      </form>|}]

module StringMap = Map.Make(String)

(* TODO: load from file *)
let token =
  In_channel.with_open_bin "/dev/urandom" (fun ch ->
    In_channel.really_input_string ch 32
  ) |> Option.get |> Hex.of_string |> Hex.show


let admin = "admin"

let login : Current_web.Resource.t =
  object
    method get_raw site request =
      Current_web.Context.of_request ~site request
      >>= fun ctx ->
      let csrf = Current_web.Context.csrf ctx in
      Current_web.Context.respond_ok ctx [login_form csrf]

    method post_raw site request body =
      Current_web.Context.of_request ~site request
      >>= fun ctx ->
      Cohttp_lwt.Body.to_form body
      >>= fun form ->
      match (List.assoc_opt "state" form, List.assoc_opt "password" form) with
      | (None | Some []), _ ->
          Server.respond_error ~status:`Bad_request ~body:"Missing state" ()
      | _, (None | Some []) ->
          Server.respond_error ~status:`Bad_request ~body:"Missing password" ()
      | Some [state], Some [pwd] ->
          if state <> Current_web.Context.csrf ctx then
            Server.respond_error ~status:`Bad_request ~body:"Bad CSRF token" ()
          else if pwd = token then (
            let user = admin in
            Log.info (fun f -> f "Successful login for %S" user) ;
            match Current_web.User.v user with
            | Error (`Msg m) ->
                Log.warn (fun f -> f "Failed to create user: %s" m) ;
                Server.respond_error ~status:`Bad_request ~body:"Bad user" ()
            | Ok user ->
                Current_web.Context.set_user ctx user )
          else
            Server.respond_error ~status:`Unauthorized ~body:"You can't be here"
              ()
      | Some (_ :: _ :: _), _ ->
          Server.respond_error ~status:`Bad_request ~body:"Multiple state" ()
      | _, Some (_ :: _ :: _) ->
          Server.respond_error ~status:`Bad_request ~body:"Multiple password" ()

    method nav_link = None
  end

let path = "login"

let auth ~csrf:_ =
  Log.info (fun m -> m "Expecting %s" token);
  Uri.make ~path (*~query:["csrf", [csrf]]*) ()

let route = Routes.((s path /? nil) @--> login)
