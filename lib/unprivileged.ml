include Build.Make(Uncached_store)(Bwrap)(Scratch)

let v ~path =
  let open Lwt.Syntax in
  let store = Fpath.(path / "store" |> to_string)
  and state_dir = Fpath.(path / "state" |> to_string) in
  let store = Uncached_store.create ~path:store () in
  let+ sandbox = Bwrap.create ~state_dir Bwrap.ro_root in
  v ~store ~sandbox
  
