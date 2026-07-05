(** Matching engine with price-time priority, partial fills,
    self-trade prevention, and balance checks. *)

type fill = {
  buyer : string;
  seller : string;
  price : float;
  quantity : float;
  maker_order_id : string;
  taker_order_id : string;
}

let fill_to_yojson (f : fill) : Yojson.Safe.t =
  `Assoc
    [
      ("buyer", `String f.buyer);
      ("seller", `String f.seller);
      ("price", `Float f.price);
      ("quantity", `Float f.quantity);
      ("maker_order_id", `String f.maker_order_id);
      ("taker_order_id", `String f.taker_order_id);
    ]

type t = {
  books : (string, Orderbook.t) Hashtbl.t;
  balances : Settlement.balances;
  mutable timestamp : int;
}

let create () =
  { books = Hashtbl.create 8; balances = Settlement.create (); timestamp = 0 }

let get_or_create_book (engine : t) ~base_token ~quote_token =
  let key = base_token ^ "/" ^ quote_token in
  match Hashtbl.find_opt engine.books key with
  | Some b -> b
  | None ->
      let b = Orderbook.create ~base_token ~quote_token in
      Hashtbl.replace engine.books key b;
      b

let next_timestamp (engine : t) =
  engine.timestamp <- engine.timestamp + 1;
  engine.timestamp

(** Check if a trader has sufficient balance to place an order *)
let check_balance (engine : t) (order : Order.t) =
  match order.side with
  | Bid ->
      (* Buyer needs quote tokens (price * quantity) *)
      let needed = order.price *. order.quantity in
      Settlement.has_sufficient_balance engine.balances ~trader:order.trader
        ~token:order.quote_token ~amount:needed
  | Ask ->
      (* Seller needs base tokens (quantity) *)
      Settlement.has_sufficient_balance engine.balances ~trader:order.trader
        ~token:order.base_token ~amount:order.quantity

(** Match a new incoming order against the book. Returns fills. *)
let match_order (engine : t) (book : Orderbook.t) (order : Order.t) :
    fill list =
  let fills = ref [] in
  let remaining = ref order.quantity in
  let continue = ref true in
  while !remaining > 1e-10 && !continue do
    let best =
      match order.side with
      | Bid -> Orderbook.best_ask book
      | Ask -> Orderbook.best_bid book
    in
    match best with
    | None -> continue := false
    | Some maker ->
        let price_match =
          match order.side with
          | Bid -> order.price >= maker.price
          | Ask -> order.price <= maker.price
        in
        if not price_match then continue := false
        else if order.trader = maker.trader then (
          (* Self-trade prevention: skip this maker order, remove it *)
          let _ =
            match order.side with
            | Bid -> Orderbook.remove_best_ask book
            | Ask -> Orderbook.remove_best_bid book
          in
          ())
        else
          let fill_qty = Float.min !remaining maker.quantity in
          let fill_price = maker.price in
          let buyer, seller =
            match order.side with
            | Bid -> (order.trader, maker.trader)
            | Ask -> (maker.trader, order.trader)
          in
          (* Attempt atomic settlement *)
          let settled =
            Settlement.settle_trade engine.balances ~buyer ~seller
              ~base_token:order.base_token ~quote_token:order.quote_token
              ~quantity:fill_qty ~price:fill_price
          in
          if settled then (
            (* The resting maker's funds were reserved when it was placed;
               release the portion backing this fill so it is not double
               counted against its available balance. *)
            (match order.side with
            | Bid ->
                (* maker is a resting ask: base was reserved *)
                Settlement.unlock engine.balances ~trader:maker.trader
                  ~token:order.base_token ~amount:fill_qty
            | Ask ->
                (* maker is a resting bid: quote was reserved at maker.price *)
                Settlement.unlock engine.balances ~trader:maker.trader
                  ~token:order.quote_token ~amount:(fill_qty *. maker.price));
            fills :=
              {
                buyer;
                seller;
                price = fill_price;
                quantity = fill_qty;
                maker_order_id = maker.id;
                taker_order_id = order.id;
              }
              :: !fills;
            remaining := !remaining -. fill_qty;
            maker.quantity <- maker.quantity -. fill_qty;
            if maker.quantity < 1e-10 then
              let _ =
                match order.side with
                | Bid -> Orderbook.remove_best_ask book
                | Ask -> Orderbook.remove_best_bid book
              in
              ())
          else (* Settlement failed, stop matching *)
            continue := false
  done;
  order.quantity <- !remaining;
  List.rev !fills

type place_result = {
  order_id : string;
  fills : fill list;
  status : string;
  message : string;
}

let place_order (engine : t) ~id ~trader ~side ~price ~quantity ~base_token
    ~quote_token : place_result =
  let ts = next_timestamp engine in
  let order =
    Order.create ~id ~trader ~side ~price ~quantity ~timestamp:ts ~base_token
      ~quote_token
  in
  (* Check balance *)
  if not (check_balance engine order) then
    {
      order_id = id;
      fills = [];
      status = "rejected";
      message = "insufficient balance";
    }
  else
    let book = get_or_create_book engine ~base_token ~quote_token in
    let fills = match_order engine book order in
    if order.quantity > 1e-10 then (
      (* Remaining quantity rests on the book. Lock the funds backing it so
         subsequent orders see a reduced available balance and settlement can
         no longer fail mid-match from over-commitment. *)
      (match side with
      | Order.Bid ->
          Settlement.lock engine.balances ~trader ~token:quote_token
            ~amount:(price *. order.quantity)
      | Order.Ask ->
          Settlement.lock engine.balances ~trader ~token:base_token
            ~amount:order.quantity);
      Orderbook.add_order book order;
      {
        order_id = id;
        fills;
        status = "partial";
        message = "order partially filled and resting";
      })
    else
      {
        order_id = id;
        fills;
        status = "filled";
        message = "order fully filled";
      }

let cancel_order (engine : t) ~order_id ~base_token ~quote_token =
  let book = get_or_create_book engine ~base_token ~quote_token in
  match Orderbook.remove_order book order_id with
  | Some (o : Order.t) ->
      (* Release the funds reserved for the cancelled resting order. *)
      (match o.side with
      | Order.Bid ->
          Settlement.unlock engine.balances ~trader:o.trader ~token:o.quote_token
            ~amount:(o.price *. o.quantity)
      | Order.Ask ->
          Settlement.unlock engine.balances ~trader:o.trader ~token:o.base_token
            ~amount:o.quantity);
      `Assoc [ ("status", `String "cancelled"); ("id", `String order_id) ]
  | None ->
      `Assoc
        [
          ("status", `String "error");
          ("message", `String "order not found");
          ("id", `String order_id);
        ]

let get_book (engine : t) ~base_token ~quote_token =
  let book = get_or_create_book engine ~base_token ~quote_token in
  Orderbook.to_yojson book

let get_balances (engine : t) ~trader =
  Settlement.trader_balances_to_yojson engine.balances ~trader

let deposit (engine : t) ~trader ~token ~amount =
  Settlement.deposit engine.balances ~trader ~token ~amount

let place_result_to_yojson (r : place_result) : Yojson.Safe.t =
  `Assoc
    [
      ("order_id", `String r.order_id);
      ("status", `String r.status);
      ("message", `String r.message);
      ("fills", `List (List.map fill_to_yojson r.fills));
    ]
