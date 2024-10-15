type kojitag = Kojitag of string [@@unboxed]

module Ticket = struct
  type t = Duration.t
  let id = "krenew"
  module Key = Current.Unit
  module Value = Current.Unit
  let pp = Fmt.any ""
  let auto_cancel = false

  let build duration job () =
    let open Lwt.Syntax in
    let run fmt =
      let f cmd =
        Current.Process.exec ~job ~cancellable:true ("", cmd |> String.split_on_char ' ' |> Array.of_list)
      in
      Format.kasprintf f fmt
    in
    let* () = Current.Job.start ~level:Current.Level.Mostly_harmless job in
    let open Lwt_result.Syntax in
    let klist () = run "klist -fe" in
    let* () = klist () in
    let* () = run "env KRB5_TRACE=/dev/stderr krenew -H %u -v" (Duration.to_min duration * 2) in
    let* () = run "koji moshimoshi" in
    klist ()
end
module TicketCache = Current_cache.Make(Ticket)

let krenew =
  let open Current.Syntax in
  let valid_for = Duration.of_min 10 in
  let schedule = Current_cache.Schedule.v ~valid_for () in
  Current.component "krenew"
  |> let> () = Current.return () in
  TicketCache.get ~schedule valid_for ()

let kojitag tag =
  Current.map (fun () -> Kojitag tag) krenew
