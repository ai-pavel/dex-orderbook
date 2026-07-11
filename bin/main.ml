(** CLI entry point: reads newline-delimited JSON commands from stdin. *)

open Dex_orderbook

let () =
  let engine = Matching_engine.create () in
  try
    while true do
      let line = input_line stdin in
      let result =
        try
          let json = Yojson.Safe.from_string line in
          Cli.handle_command engine json
        with
        | Failure msg -> `Assoc [ ("error", `String msg) ]
        | Yojson.Json_error msg -> `Assoc [ ("error", `String ("JSON parse error: " ^ msg)) ]
      in
      print_endline (Yojson.Safe.to_string result)
    done
  with End_of_file -> ()