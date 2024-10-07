let fetch ~log:_ ~rootfs = function
| "scratch" ->
  Os.ensure_dir rootfs;
  Unix.environment ()
  |> Array.to_list
  |> List.filter_map (Astring.String.cut ~sep:"=")
  |> Lwt.return
| image ->
  Fmt.invalid_arg "Only the special 'scratch' image is supported by this fetcher, got: %S" image
