(** CLI: reads newline-delimited JSON commands from stdin, writes JSON to stdout. *)

open Dex_orderbook

let get_string json key =
  match json with
  | `Assoc l -> (
      match List.assoc_opt key l with
      | Some (`String s) -> s
      | _ -> failwith ("missing string field: " ^ key))
  | _ -> failwith "expected object"

let get_float json key =
  match json with
  | `Assoc l -> (
      match List.assoc_opt key l with
      | Some (`Float f) -> f
      | Some (`Int i) -> Float.of_int i
      | _ -> failwith ("missing float field: " ^ key))
  | _ -> failwith "expected object"

let handle_command engine json =
  let cmd = get_string json "command" in
  match cmd with
  | "deposit" ->
      let trader = get_string json "trader" in
      let token = get_string json "token" in
      let amount = get_float json "amount" in
      Matching_engine.deposit engine ~trader ~token ~amount;
      `Assoc
        [
          ("status", `String "ok");
          ("command", `String "deposit");
          ("trader", `String trader);
          ("token", `String token);
          ("amount", `Float amount);
        ]
  | "place_order" ->
      let id = get_string json "id" in
      let trader = get_string json "trader" in
      let side = Order.side_of_string (get_string json "side") in
      let price = get_float json "price" in
      let quantity = get_float json "quantity" in
      let base_token = get_string json "base" in
      let quote_token = get_string json "quote" in
      let result =
        Matching_engine.place_order engine ~id ~trader ~side ~price ~quantity
          ~base_token ~quote_token
      in
      Matching_engine.place_result_to_yojson result
  | "cancel_order" ->
      let order_id = get_string json "id" in
      let base_token = get_string json "base" in
      let quote_token = get_string json "quote" in
      Matching_engine.cancel_order engine ~order_id ~base_token ~quote_token
  | "replace_order" ->
      (* Cancel the resting order [id] and place a new order [new_id].
         The replacement resets time priority (fresh timestamp). *)
      let order_id = get_string json "id" in
      let new_id = get_string json "new_id" in
      let trader = get_string json "trader" in
      let side = Order.side_of_string (get_string json "side") in
      let price = get_float json "price" in
      let quantity = get_float json "quantity" in
      let base_token = get_string json "base" in
      let quote_token = get_string json "quote" in
      let result =
        Matching_engine.replace_order engine ~order_id ~new_id ~trader ~side
          ~price ~quantity ~base_token ~quote_token
      in
      Matching_engine.place_result_to_yojson result
  | "get_book" ->
      let base_token = get_string json "base" in
      let quote_token = get_string json "quote" in
      Matching_engine.get_book engine ~base_token ~quote_token
  | "get_balances" ->
      let trader = get_string json "trader" in
      Matching_engine.get_balances engine ~trader
  | _ -> `Assoc [ ("error", `String ("unknown command: " ^ cmd)) ]

let () =
  let engine = Matching_engine.create () in
  try
    while true do
      let line = input_line stdin in
      let result =
        try
          let json = Yojson.Safe.from_string line in
          handle_command engine json
        with
        | Failure msg -> `Assoc [ ("error", `String msg) ]
        | Yojson.Json_error msg -> `Assoc [ ("error", `String ("JSON parse error: " ^ msg)) ]
      in
      print_endline (Yojson.Safe.to_string result)
    done
  with End_of_file -> ()
