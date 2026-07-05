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

(* Test: resting orders reserve funds so a trader cannot over-commit *)
let test_reserved_over_commitment () =
  let engine = Matching_engine.create () in
  (* Carol can afford exactly one 10-ETH bid at 100 (1000 USDC). *)
  Matching_engine.deposit engine ~trader:"carol" ~token:"USDC" ~amount:1000.0;
  let r1 =
    Matching_engine.place_order engine ~id:"1" ~trader:"carol" ~side:Order.Bid
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check string) "first bid rests" "partial" r1.status;
  (* Second identical bid must be rejected: the 1000 USDC is now reserved. *)
  let r2 =
    Matching_engine.place_order engine ~id:"2" ~trader:"carol" ~side:Order.Bid
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check string) "second bid rejected" "rejected" r2.status;
  (* Cancelling the first releases the reservation, allowing a new bid. *)
  let _ =
    Matching_engine.cancel_order engine ~order_id:"1" ~base_token:"ETH"
      ~quote_token:"USDC"
  in
  let r3 =
    Matching_engine.place_order engine ~id:"3" ~trader:"carol" ~side:Order.Bid
      ~price:100.0 ~quantity:10.0 ~base_token:"ETH" ~quote_token:"USDC"
  in
  Alcotest.(check string) "bid after cancel rests" "partial" r3.status

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
          Alcotest.test_case "reserved over-commitment" `Quick
            test_reserved_over_commitment;
        ] );
    ]
