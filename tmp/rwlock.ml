open Lwt.Syntax

type t =
  { readcount: int Atomic.t
  ; resource: Lwt_mutex.t
  ; service_queue: Lwt_mutex.t (* Lwt_mutex is FIFO *) }

let create () =
  { readcount= Atomic.make 0
  ; resource= Lwt_mutex.create ()
  ; service_queue= Lwt_mutex.create () }

let[@inline always] with_lock ~enter ~exit t fn =
  let* () = Lwt_mutex.with_lock t.service_queue (fun () -> enter t) in
  Lwt.finalize fn (fun () -> exit t ; Lwt.return_unit)

let[@inline always] reader_enter t =
  (* no need for an 'rmutex' here: Lwt only switches execution at binding points *)
  if Atomic.fetch_and_add t.readcount 1 = 0 then
    (* first reader: block writers *)
    Lwt_mutex.lock t.resource
  else Lwt.return_unit

let[@inline always] reader_exit t =
  if Atomic.fetch_and_add t.readcount (-1) = 1 then
    (* last reader *)
    Lwt_mutex.unlock t.resource

let with_read t fn = with_lock ~enter:reader_enter ~exit:reader_exit t fn

let[@inline always] writer_enter t = Lwt_mutex.lock t.resource

let[@inline always] writer_exit t = Lwt_mutex.unlock t.resource

let with_write t fn = with_lock ~enter:writer_enter ~exit:writer_exit t fn
