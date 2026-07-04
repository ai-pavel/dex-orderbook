type side = Bid | Ask

type t = {
  id : string;
  trader : string;
  side : side;
  price : float;
  mutable quantity : float;
  timestamp : int;
  base_token : string;
  quote_token : string;
}

let side_to_string = function Bid -> "bid" | Ask -> "ask"

let side_of_string = function
  | "bid" -> Bid
  | "ask" -> Ask
  | s -> failwith ("Unknown side: " ^ s)

let create ~id ~trader ~side ~price ~quantity ~timestamp ~base_token ~quote_token =
  { id; trader; side; price; quantity; timestamp; base_token; quote_token }

let to_yojson (o : t) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `String o.id);
      ("trader", `String o.trader);
      ("side", `String (side_to_string o.side));
      ("price", `Float o.price);
      ("quantity", `Float o.quantity);
      ("timestamp", `Int o.timestamp);
      ("base", `String o.base_token);
      ("quote", `String o.quote_token);
    ]

let pair_key (o : t) = o.base_token ^ "/" ^ o.quote_token
