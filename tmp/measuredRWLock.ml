open Prometheus

let namespace = "ocurrent"

let subsystem = "rwlock"

type lock_metric = {wait: string -> Summary.t; hold: string -> Summary.t}

let lock_metrics kind =
  let wait =
    let help = Printf.sprintf "Time waiting to acquire a %s lock" kind
    and label = Printf.sprintf "%s_lock_wait_time" kind in
    Summary.v_label ~help ~label_name:"name" ~namespace ~subsystem label
  and hold =
    let help = Printf.sprintf "Time holding %s lock" kind
    and label = Printf.sprintf "%s_lock_hold_time" kind in
    Summary.v_label ~help ~label_name:"name" ~namespace ~subsystem label
  in
  {wait; hold}

let write = lock_metrics "write"

let read = lock_metrics "read"

type lock_metrics = {wait: Summary.t; hold: Summary.t}

let metrics_for name (def : lock_metric) : lock_metrics =
  {wait= def.wait name; hold= def.hold name}

type t = {lock: Rwlock.t; read: lock_metrics; write: lock_metrics}

let create name =
  { lock= Rwlock.create ()
  ; read= metrics_for name read
  ; write= metrics_for name write }

let measure with_lock t fn =
  let t0 = Mtime_clock.counter () in
  with_lock t.lock
  @@ fun () ->
  let dt = Mtime_clock.count t0 and t1 = Mtime_clock.counter () in
  Prometheus.Summary.observe t.read.wait (Mtime.Span.to_float_ns dt *. 1e-9) ;
  Lwt.finalize fn (fun () ->
      let dt = Mtime_clock.count t1 in
      Prometheus.Summary.observe t.read.hold (Mtime.Span.to_float_ns dt *. 1e-9) ;
      Lwt.return_unit )

let with_read t fn = measure Rwlock.with_read t fn

let with_write t fn = measure Rwlock.with_write t fn
