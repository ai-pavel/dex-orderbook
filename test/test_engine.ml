open Dex_orderbook

let engine_with_deposits () =
  let engine = Matching_engine.create () in
  Matching_engine.deposit engine ~trader:"alice" ~token:"ETH" ~amount:100.0;
  Matching_engine.deposit engine ~trader:"alice" ~token:"USDC" ~amount:100000.0;
  Matching_engine.deposit engine ~trader:"bob" ~token:"ETH" ~amount:100.0;
  Matching_engine.deposit engine ~trader:"bob" ~token:"USDC" ~amount:100000.0;
  engine

(* Test: exact match - bid and ask at same price and quantity *)
let test_exact_match () =
  let engine = engine_with_deposits () in
  (* Alice places an ask at 100.0 for 10 ETH *)
  let r1 =
    Matching_engine.place_order engine ~id:"1" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check string) "ask rests" "partial" r1.status;
  Alcotest.(check (list pass)) "no fills on ask" [] r1.fills;
  (* Bob places a bid at 100.0 for 10 ETH - should match exactly *)
  let r2 =
    Matching_engine.place_order engine ~id:"2" ~trader:"bob" ~side:Order.Bid
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check string) "bid filled" "filled" r2.status;
  Alcotest.(check int) "one fill" 1 (List.length r2.fills);
  let fill = List.hd r2.fills in
  Alcotest.(check (float 0.01)) "fill qty" 10.0 fill.quantity;
  Alcotest.(check (float 0.01)) "fill price" 100.0 fill.price;
  Alcotest.(check string) "buyer is bob" "bob" fill.buyer;
  Alcotest.(check string) "seller is alice" "alice" fill.seller

(* Test: partial fill *)
let test_partial_fill () =
  let engine = engine_with_deposits () in
  (* Alice asks 10 ETH at 100 *)
  let _ =
    Matching_engine.place_order engine ~id:"1" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  (* Bob bids 5 ETH at 100 - partial fill *)
  let r2 =
    Matching_engine.place_order engine ~id:"2" ~trader:"bob" ~side:Order.Bid
      ~price:100.0 ~quantity:5.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check string) "bid fully filled" "filled" r2.status;
  Alcotest.(check int) "one fill" 1 (List.length r2.fills);
  let fill = List.hd r2.fills in
  Alcotest.(check (float 0.01)) "fill qty" 5.0 fill.quantity;
  (* Check book still has alice's remaining 5 *)
  let book_json = Matching_engine.get_book engine ~base_token:"ETH" ~quote_token:"USDC" in
  match book_json with
  | `Assoc l -> (
      match List.assoc "asks" l with
      | `List [ `Assoc ask ] -> (
          match List.assoc "quantity" ask with
          | `Float q -> Alcotest.(check (float 0.01)) "remaining ask" 5.0 q
          | _ -> Alcotest.fail "bad quantity")
      | _ -> Alcotest.fail "expected one remaining ask")
  | _ -> Alcotest.fail "bad book json"

(* Test: self-trade prevention *)
let test_self_trade_prevention () =
  let engine = engine_with_deposits () in
  (* Alice places ask *)
  let _ =
    Matching_engine.place_order engine ~id:"1" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  (* Alice places bid at matching price - self-trade should be prevented *)
  let r =
    Matching_engine.place_order engine ~id:"2" ~trader:"alice" ~side:Order.Bid
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check (list pass)) "no fills (self-trade prevented)" [] r.fills;
  Alcotest.(check string) "order rests" "partial" r.status

(* Test: insufficient balance rejection *)
let test_insufficient_balance () =
  let engine = Matching_engine.create () in
  (* Bob has no deposits *)
  let r =
    Matching_engine.place_order engine ~id:"1" ~trader:"bob" ~side:Order.Bid
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check string) "rejected" "rejected" r.status;
  Alcotest.(check string) "reason" "insufficient balance" r.message

(* Test: cancel order *)
let test_cancel_order () =
  let engine = engine_with_deposits () in
  let _ =
    Matching_engine.place_order engine ~id:"1" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let result =
    Matching_engine.cancel_order engine ~order_id:"1" ~base_token:"ETH"
      ~quote_token:"USDC"
  in
  (match result with
  | `Assoc l -> (
      match List.assoc_opt "status" l with
      | Some (`String s) -> Alcotest.(check string) "cancelled" "cancelled" s
      | _ -> Alcotest.fail "expected status")
  | _ -> Alcotest.fail "bad result");
  (* Book should be empty *)
  let book = Matching_engine.get_book engine ~base_token:"ETH" ~quote_token:"USDC" in
  match book with
  | `Assoc l -> (
      match List.assoc "asks" l with
      | `List [] -> ()
      | _ -> Alcotest.fail "expected empty asks")
  | _ -> Alcotest.fail "bad book"

(* Test: price-time priority *)
let test_price_time_priority () =
  let engine = engine_with_deposits () in
  (* Two asks at different prices *)
  let _ =
    Matching_engine.place_order engine ~id:"1" ~trader:"alice" ~side:Order.Ask
      ~price:102.0 ~quantity:5.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let _ =
    Matching_engine.place_order engine ~id:"2" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:5.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  (* Bob's bid should match the lower ask first *)
  let r =
    Matching_engine.place_order engine ~id:"3" ~trader:"bob" ~side:Order.Bid
      ~price:105.0 ~quantity:5.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check int) "one fill" 1 (List.length r.fills);
  let fill = List.hd r.fills in
  Alcotest.(check (float 0.01)) "fills at lower price" 100.0 fill.price;
  Alcotest.(check string) "maker is order 2" "2" fill.maker_order_id

(* Test: balances update correctly after trade *)
let test_balance_update () =
  let engine = engine_with_deposits () in
  let _ =
    Matching_engine.place_order engine ~id:"1" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let _ =
    Matching_engine.place_order engine ~id:"2" ~trader:"bob" ~side:Order.Bid
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  (* Alice sold 10 ETH at 100 USDC each: -10 ETH, +1000 USDC *)
  let alice_eth =
    Settlement.get_balance engine.balances ~trader:"alice" ~token:"ETH"
  in
  let alice_usdc =
    Settlement.get_balance engine.balances ~trader:"alice" ~token:"USDC"
  in
  Alcotest.(check (float 0.01)) "alice ETH" 90.0 alice_eth;
  Alcotest.(check (float 0.01)) "alice USDC" 101000.0 alice_usdc;
  (* Bob bought 10 ETH: +10 ETH, -1000 USDC *)
  let bob_eth =
    Settlement.get_balance engine.balances ~trader:"bob" ~token:"ETH"
  in
  let bob_usdc =
    Settlement.get_balance engine.balances ~trader:"bob" ~token:"USDC"
  in
  Alcotest.(check (float 0.01)) "bob ETH" 110.0 bob_eth;
  Alcotest.(check (float 0.01)) "bob USDC" 99000.0 bob_usdc

(* Test: a taker order sweeps multiple price levels in one call *)
let test_multi_level_sweep () =
  let engine = engine_with_deposits () in
  (* Alice rests three asks at rising prices. *)
  let _ =
    Matching_engine.place_order engine ~id:"a1" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:5.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let _ =
    Matching_engine.place_order engine ~id:"a2" ~trader:"alice" ~side:Order.Ask
      ~price:101.0 ~quantity:5.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let _ =
    Matching_engine.place_order engine ~id:"a3" ~trader:"alice" ~side:Order.Ask
      ~price:102.0 ~quantity:5.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  (* Bob's aggressive bid sweeps the first two levels and part of the third. *)
  let r =
    Matching_engine.place_order engine ~id:"b1" ~trader:"bob" ~side:Order.Bid
      ~price:102.0 ~quantity:12.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check string) "bid fully filled" "filled" r.status;
  Alcotest.(check int) "three fills across levels" 3 (List.length r.fills);
  let prices = List.map (fun (f : Matching_engine.fill) -> f.price) r.fills in
  (match prices with
  | [ p0; p1; p2 ] ->
      Alcotest.(check (float 0.01)) "level 1 price" 100.0 p0;
      Alcotest.(check (float 0.01)) "level 2 price" 101.0 p1;
      Alcotest.(check (float 0.01)) "level 3 price" 102.0 p2
  | _ -> Alcotest.fail "expected exactly three fills");
  let qtys = List.map (fun (f : Matching_engine.fill) -> f.quantity) r.fills in
  (match qtys with
  | [ q0; q1; q2 ] ->
      Alcotest.(check (float 0.01)) "level 1 qty" 5.0 q0;
      Alcotest.(check (float 0.01)) "level 2 qty" 5.0 q1;
      Alcotest.(check (float 0.01)) "level 3 partial qty" 2.0 q2
  | _ -> Alcotest.fail "expected exactly three fills")

(* Test: cancelling a nonexistent order returns the 'order not found' branch *)
let test_cancel_not_found () =
  let engine = engine_with_deposits () in
  (* Create the book by resting an unrelated order, then cancel a bad id. *)
  let _ =
    Matching_engine.place_order engine ~id:"1" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:1.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let result =
    Matching_engine.cancel_order engine ~order_id:"does-not-exist"
      ~base_token:"ETH" ~quote_token:"USDC"
  in
  match result with
  | `Assoc l ->
      (match List.assoc_opt "status" l with
      | Some (`String s) -> Alcotest.(check string) "error status" "error" s
      | _ -> Alcotest.fail "expected status");
      (match List.assoc_opt "message" l with
      | Some (`String m) ->
          Alcotest.(check string) "message" "order not found" m
      | _ -> Alcotest.fail "expected message")
  | _ -> Alcotest.fail "bad result"

(* Test: total per-token balances are conserved across a sequence of trades *)
let test_value_conservation () =
  let engine = engine_with_deposits () in
  let total token =
    Settlement.get_balance engine.balances ~trader:"alice" ~token
    +. Settlement.get_balance engine.balances ~trader:"bob" ~token
  in
  let eth0 = total "ETH" in
  let usdc0 = total "USDC" in
  (* A sequence of crossing trades between alice and bob. *)
  let _ =
    Matching_engine.place_order engine ~id:"1" ~trader:"alice" ~side:Order.Ask
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let _ =
    Matching_engine.place_order engine ~id:"2" ~trader:"bob" ~side:Order.Bid
      ~price:100.0 ~quantity:4.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let _ =
    Matching_engine.place_order engine ~id:"3" ~trader:"bob" ~side:Order.Bid
      ~price:101.0 ~quantity:3.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  let _ =
    Matching_engine.place_order engine ~id:"4" ~trader:"alice" ~side:Order.Ask
      ~price:99.0 ~quantity:2.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check (float 0.001)) "ETH conserved" eth0 (total "ETH");
  Alcotest.(check (float 0.001)) "USDC conserved" usdc0 (total "USDC")

let () =
  Alcotest.run "DEX Orderbook"
    [
      ( "matching",
        [
          Alcotest.test_case "exact match" `Quick test_exact_match;
          Alcotest.test_case "partial fill" `Quick test_partial_fill;
          Alcotest.test_case "self-trade prevention" `Quick
            test_self_trade_prevention;
          Alcotest.test_case "insufficient balance" `Quick
            test_insufficient_balance;
          Alcotest.test_case "cancel order" `Quick test_cancel_order;
          Alcotest.test_case "price-time priority" `Quick
            test_price_time_priority;
          Alcotest.test_case "balance update" `Quick test_balance_update;
          Alcotest.test_case "multi-level sweep" `Quick test_multi_level_sweep;
          Alcotest.test_case "cancel not found" `Quick test_cancel_not_found;
          Alcotest.test_case "value conservation" `Quick
            test_value_conservation;
        ] );
    ]
