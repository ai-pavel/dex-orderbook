open Dex_orderbook

(* --- Order tests --- *)

let test_side_to_string () =
  Alcotest.(check string) "bid" "bid" (Order.side_to_string Order.Bid);
  Alcotest.(check string) "ask" "ask" (Order.side_to_string Order.Ask)

let test_side_of_string_valid () =
  Alcotest.(check (option pass)) "bid" (Some Order.Bid)
    (try Some (Order.side_of_string "bid") with _ -> None);
  Alcotest.(check (option pass)) "ask" (Some Order.Ask)
    (try Some (Order.side_of_string "ask") with _ -> None)

let test_side_of_string_invalid () =
  try
    let _ = Order.side_of_string "invalid" in
    Alcotest.fail "expected failure"
  with Failure _ -> ()

let test_order_to_yojson () =
  let o =
    Order.create ~id:"o1" ~trader:"alice" ~side:Order.Bid ~price:100.0
      ~quantity:5.0 ~timestamp:1 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let json = Order.to_yojson o in
  match json with
  | `Assoc l ->
    Alcotest.(check string) "id" "o1" (List.assoc "id" l |> function `String s -> s | _ -> "");
    Alcotest.(check string) "trader" "alice" (List.assoc "trader" l |> function `String s -> s | _ -> "");
    Alcotest.(check string) "side" "bid" (List.assoc "side" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad json"

let test_pair_key () =
  let o =
    Order.create ~id:"o1" ~trader:"alice" ~side:Order.Bid ~price:100.0
      ~quantity:5.0 ~timestamp:1 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check string) "pair" "ETH/USDC" (Order.pair_key o)

(* --- Orderbook edge tests --- *)

let test_empty_book_best_bid () =
  let book = Orderbook.create ~base_token:"ETH" ~quote_token:"USDC" in
  Alcotest.(check (option pass)) "no best bid" None (Orderbook.best_bid book)

let test_empty_book_best_ask () =
  let book = Orderbook.create ~base_token:"ETH" ~quote_token:"USDC" in
  Alcotest.(check (option pass)) "no best ask" None (Orderbook.best_ask book)

let test_remove_best_bid_empty () =
  let book = Orderbook.create ~base_token:"ETH" ~quote_token:"USDC" in
  Alcotest.(check (option pass)) "empty" None (Orderbook.remove_best_bid book)

let test_remove_best_ask_empty () =
  let book = Orderbook.create ~base_token:"ETH" ~quote_token:"USDC" in
  Alcotest.(check (option pass)) "empty" None (Orderbook.remove_best_ask book)

let test_remove_nonexistent_order () =
  let book = Orderbook.create ~base_token:"ETH" ~quote_token:"USDC" in
  Alcotest.(check (option pass)) "not found" None (Orderbook.remove_order book "nonexistent")

let test_remove_order_from_asks () =
  let book = Orderbook.create ~base_token:"ETH" ~quote_token:"USDC" in
  let o =
    Order.create ~id:"a1" ~trader:"alice" ~side:Order.Ask ~price:100.0
      ~quantity:5.0 ~timestamp:1 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Orderbook.add_order book o;
  Alcotest.(check (option pass)) "found" (Some ()) (Orderbook.remove_order book "a1" |> Option.map (fun _ -> ()))

let test_orderbook_to_yojson () =
  let book = Orderbook.create ~base_token:"ETH" ~quote_token:"USDC" in
  let json = Orderbook.to_yojson book in
  match json with
  | `Assoc l ->
    Alcotest.(check string) "pair" "ETH/USDC" (List.assoc "pair" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad json"

(* --- Settlement edge tests --- *)

let test_settle_insufficient_buyer () =
  let b = Settlement.create () in
  Settlement.deposit b ~trader:"seller" ~token:"ETH" ~amount:100.0;
  (* buyer has no quote tokens *)
  Alcotest.(check bool) "settle fails" false
    (Settlement.settle_trade b ~buyer:"buyer" ~seller:"seller"
       ~base_token:"ETH" ~quote_token:"USDC" ~quantity:10.0 ~price:100.0)

let test_settle_insufficient_seller () =
  let b = Settlement.create () in
  Settlement.deposit b ~trader:"buyer" ~token:"USDC" ~amount:100000.0;
  (* seller has no base tokens *)
  Alcotest.(check bool) "settle fails" false
    (Settlement.settle_trade b ~buyer:"buyer" ~seller:"seller"
       ~base_token:"ETH" ~quote_token:"USDC" ~quantity:10.0 ~price:100.0)

let test_get_balance_unknown_trader () =
  let b = Settlement.create () in
  Alcotest.(check (float 0.01)) "zero" 0.0 (Settlement.get_balance b ~trader:"nobody" ~token:"ETH")

let test_get_balance_unknown_token () =
  let b = Settlement.create () in
  Settlement.deposit b ~trader:"alice" ~token:"ETH" ~amount:100.0;
  Alcotest.(check (float 0.01)) "zero" 0.0 (Settlement.get_balance b ~trader:"alice" ~token:"USDC")

let test_trader_balances_unknown () =
  let b = Settlement.create () in
  let json = Settlement.trader_balances_to_yojson b ~trader:"nobody" in
  match json with
  | `Assoc l ->
    Alcotest.(check string) "trader" "nobody" (List.assoc "trader" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad json"

let test_trader_balances_known () =
  let b = Settlement.create () in
  Settlement.deposit b ~trader:"alice" ~token:"ETH" ~amount:100.0;
  Settlement.deposit b ~trader:"alice" ~token:"USDC" ~amount:500.0;
  let json = Settlement.trader_balances_to_yojson b ~trader:"alice" in
  match json with
  | `Assoc l ->
    (match List.assoc "balances" l with
     | `Assoc items ->
       Alcotest.(check int) "two tokens" 2 (List.length items)
     | _ -> Alcotest.fail "bad balances")
  | _ -> Alcotest.fail "bad json"

let test_has_sufficient_balance () =
  let b = Settlement.create () in
  Settlement.deposit b ~trader:"alice" ~token:"ETH" ~amount:100.0;
  Alcotest.(check bool) "sufficient" true
    (Settlement.has_sufficient_balance b ~trader:"alice" ~token:"ETH" ~amount:100.0);
  Alcotest.(check bool) "insufficient" false
    (Settlement.has_sufficient_balance b ~trader:"alice" ~token:"ETH" ~amount:101.0);
  Alcotest.(check bool) "unknown trader" false
    (Settlement.has_sufficient_balance b ~trader:"bob" ~token:"ETH" ~amount:1.0)

(* --- Matching engine edge tests --- *)

let test_cancel_nonexistent_order () =
  let engine = Matching_engine.create () in
  let result = Matching_engine.cancel_order engine ~order_id:"nope" ~base_token:"ETH" ~quote_token:"USDC" in
  match result with
  | `Assoc l ->
    Alcotest.(check string) "error" "error" (List.assoc "status" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad result"

let test_get_balances () =
  let engine = Matching_engine.create () in
  Matching_engine.deposit engine ~trader:"alice" ~token:"ETH" ~amount:100.0;
  let json = Matching_engine.get_balances engine ~trader:"alice" in
  match json with
  | `Assoc l ->
    Alcotest.(check string) "trader" "alice" (List.assoc "trader" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad json"

let test_get_balances_unknown () =
  let engine = Matching_engine.create () in
  let json = Matching_engine.get_balances engine ~trader:"nobody" in
  match json with
  | `Assoc l ->
    Alcotest.(check string) "trader" "nobody" (List.assoc "trader" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad json"

let test_ask_self_trade_prevention () =
  let engine = Matching_engine.create () in
  Matching_engine.deposit engine ~trader:"alice" ~token:"ETH" ~amount:100.0;
  Matching_engine.deposit engine ~trader:"alice" ~token:"USDC" ~amount:100000.0;
  (* Alice places a bid *)
  let _ =
    Matching_engine.place_order engine ~id:"1" ~trader:"alice" ~side:Order.Bid
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  (* Alice places an ask at matching price - self-trade should be prevented *)
  let r =
    Matching_engine.place_order engine ~id:"2" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check (list pass)) "no fills" [] r.fills;
  Alcotest.(check string) "order rests" "partial" r.status

let test_settlement_failure_during_match () =
  let engine = Matching_engine.create () in
  (* Alice has ETH but no USDC; Bob has USDC but no ETH *)
  Matching_engine.deposit engine ~trader:"alice" ~token:"ETH" ~amount:100.0;
  Matching_engine.deposit engine ~trader:"bob" ~token:"USDC" ~amount:100000.0;
  (* Alice asks 10 ETH at 100 - she has ETH so this rests *)
  let _ =
    Matching_engine.place_order engine ~id:"1" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  (* Bob bids but his ETH balance won't matter, he has USDC to pay *)
  (* Now drain alice's ETH so settlement fails for the ask side *)
  Settlement.set_balance engine.Matching_engine.balances ~trader:"alice" ~token:"ETH" ~amount:0.0;
  (* Bob bids at 100 - should fail settlement since alice has no ETH *)
  let r =
    Matching_engine.place_order engine ~id:"2" ~trader:"bob" ~side:Order.Bid
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check (list pass)) "no fills" [] r.fills

let test_place_result_to_yojson () =
  let engine = Matching_engine.create () in
  Matching_engine.deposit engine ~trader:"bob" ~token:"USDC" ~amount:100000.0;
  let r =
    Matching_engine.place_order engine ~id:"1" ~trader:"bob" ~side:Order.Bid
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let json = Matching_engine.place_result_to_yojson r in
  match json with
  | `Assoc l ->
    Alcotest.(check string) "status" "partial" (List.assoc "status" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad json"

let test_fill_to_yojson () =
  let engine = Matching_engine.create () in
  Matching_engine.deposit engine ~trader:"alice" ~token:"ETH" ~amount:100.0;
  Matching_engine.deposit engine ~trader:"bob" ~token:"USDC" ~amount:100000.0;
  let _ =
    Matching_engine.place_order engine ~id:"1" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let r =
    Matching_engine.place_order engine ~id:"2" ~trader:"bob" ~side:Order.Bid
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  match r.fills with
  | [] -> Alcotest.fail "expected fills"
  | fill :: _ ->
    let json = Matching_engine.fill_to_yojson fill in
    (match json with
     | `Assoc l ->
       Alcotest.(check string) "buyer" "bob" (List.assoc "buyer" l |> function `String s -> s | _ -> "")
     | _ -> Alcotest.fail "bad json")

(* --- CLI tests --- *)

let test_cli_deposit () =
  let engine = Matching_engine.create () in
  let json = Yojson.Safe.from_string
    {|{"command":"deposit","trader":"alice","token":"ETH","amount":100.0}|}
  in
  let result = Cli.handle_command engine json in
  match result with
  | `Assoc l ->
    Alcotest.(check string) "status" "ok" (List.assoc "status" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad result"

let test_cli_place_order () =
  let engine = Matching_engine.create () in
  Matching_engine.deposit engine ~trader:"alice" ~token:"ETH" ~amount:100.0;
  let json = Yojson.Safe.from_string
    {|{"command":"place_order","id":"1","trader":"alice","side":"ask","price":100.0,"quantity":10.0,"base":"ETH","quote":"USDC"}|}
  in
  let result = Cli.handle_command engine json in
  match result with
  | `Assoc l ->
    Alcotest.(check string) "status" "partial" (List.assoc "status" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad result"

let test_cli_cancel_order () =
  let engine = Matching_engine.create () in
  Matching_engine.deposit engine ~trader:"alice" ~token:"ETH" ~amount:100.0;
  let _ = Cli.handle_command engine (Yojson.Safe.from_string
    {|{"command":"place_order","id":"1","trader":"alice","side":"ask","price":100.0,"quantity":10.0,"base":"ETH","quote":"USDC"}|})
  in
  let result = Cli.handle_command engine (Yojson.Safe.from_string
    {|{"command":"cancel_order","id":"1","base":"ETH","quote":"USDC"}|})
  in
  match result with
  | `Assoc l ->
    Alcotest.(check string) "cancelled" "cancelled" (List.assoc "status" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad result"

let test_cli_get_book () =
  let engine = Matching_engine.create () in
  let result = Cli.handle_command engine (Yojson.Safe.from_string
    {|{"command":"get_book","base":"ETH","quote":"USDC"}|})
  in
  match result with
  | `Assoc l ->
    Alcotest.(check string) "pair" "ETH/USDC" (List.assoc "pair" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad result"

let test_cli_get_balances () =
  let engine = Matching_engine.create () in
  Matching_engine.deposit engine ~trader:"alice" ~token:"ETH" ~amount:100.0;
  let result = Cli.handle_command engine (Yojson.Safe.from_string
    {|{"command":"get_balances","trader":"alice"}|})
  in
  match result with
  | `Assoc l ->
    Alcotest.(check string) "trader" "alice" (List.assoc "trader" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad result"

let test_cli_unknown_command () =
  let engine = Matching_engine.create () in
  let result = Cli.handle_command engine (Yojson.Safe.from_string
    {|{"command":"frobnicate"}|})
  in
  match result with
  | `Assoc l ->
    Alcotest.(check string) "error" "unknown command: frobnicate" (List.assoc "error" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad result"

let test_cli_int_amount () =
  let engine = Matching_engine.create () in
  (* amount as int instead of float to test get_float int path *)
  let result = Cli.handle_command engine (Yojson.Safe.from_string
    {|{"command":"deposit","trader":"alice","token":"ETH","amount":100}|})
  in
  match result with
  | `Assoc l ->
    Alcotest.(check string) "status" "ok" (List.assoc "status" l |> function `String s -> s | _ -> "")
  | _ -> Alcotest.fail "bad result"

let () =
  Alcotest.run "DEX Orderbook - Coverage"
    [
      ("order", [
        Alcotest.test_case "side_to_string" `Quick test_side_to_string;
        Alcotest.test_case "side_of_string valid" `Quick test_side_of_string_valid;
        Alcotest.test_case "side_of_string invalid" `Quick test_side_of_string_invalid;
        Alcotest.test_case "to_yojson" `Quick test_order_to_yojson;
        Alcotest.test_case "pair_key" `Quick test_pair_key;
      ]);
      ("orderbook", [
        Alcotest.test_case "empty best bid" `Quick test_empty_book_best_bid;
        Alcotest.test_case "empty best ask" `Quick test_empty_book_best_ask;
        Alcotest.test_case "remove best bid empty" `Quick test_remove_best_bid_empty;
        Alcotest.test_case "remove best ask empty" `Quick test_remove_best_ask_empty;
        Alcotest.test_case "remove nonexistent order" `Quick test_remove_nonexistent_order;
        Alcotest.test_case "remove order from asks" `Quick test_remove_order_from_asks;
        Alcotest.test_case "to_yojson" `Quick test_orderbook_to_yojson;
      ]);
      ("settlement", [
        Alcotest.test_case "settle insufficient buyer" `Quick test_settle_insufficient_buyer;
        Alcotest.test_case "settle insufficient seller" `Quick test_settle_insufficient_seller;
        Alcotest.test_case "get balance unknown trader" `Quick test_get_balance_unknown_trader;
        Alcotest.test_case "get balance unknown token" `Quick test_get_balance_unknown_token;
        Alcotest.test_case "trader balances unknown" `Quick test_trader_balances_unknown;
        Alcotest.test_case "trader balances known" `Quick test_trader_balances_known;
        Alcotest.test_case "has sufficient balance" `Quick test_has_sufficient_balance;
      ]);
      ("matching edges", [
        Alcotest.test_case "cancel nonexistent" `Quick test_cancel_nonexistent_order;
        Alcotest.test_case "get balances" `Quick test_get_balances;
        Alcotest.test_case "get balances unknown" `Quick test_get_balances_unknown;
        Alcotest.test_case "ask self-trade prevention" `Quick test_ask_self_trade_prevention;
        Alcotest.test_case "settlement failure during match" `Quick test_settlement_failure_during_match;
        Alcotest.test_case "place_result_to_yojson" `Quick test_place_result_to_yojson;
        Alcotest.test_case "fill_to_yojson" `Quick test_fill_to_yojson;
      ]);
      ("cli", [
        Alcotest.test_case "deposit" `Quick test_cli_deposit;
        Alcotest.test_case "place_order" `Quick test_cli_place_order;
        Alcotest.test_case "cancel_order" `Quick test_cli_cancel_order;
        Alcotest.test_case "get_book" `Quick test_cli_get_book;
        Alcotest.test_case "get_balances" `Quick test_cli_get_balances;
        Alcotest.test_case "unknown command" `Quick test_cli_unknown_command;
        Alcotest.test_case "int amount" `Quick test_cli_int_amount;
      ]);
    ]