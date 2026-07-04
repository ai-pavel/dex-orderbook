(** Order book using sorted lists as priority queues.
    Bids sorted descending by price then ascending by timestamp (max-heap semantics).
    Asks sorted ascending by price then ascending by timestamp (min-heap semantics). *)

type t = {
  base_token : string;
  quote_token : string;
  mutable bids : Order.t list;
  mutable asks : Order.t list;
}

let create ~base_token ~quote_token =
  { base_token; quote_token; bids = []; asks = [] }

let pair_key (book : t) = book.base_token ^ "/" ^ book.quote_token

(** Insert a bid in descending price, ascending timestamp order *)
let insert_bid (book : t) (order : Order.t) =
  let rec insert = function
    | [] -> [ order ]
    | (h :: _) as l when order.price > h.price -> order :: l
    | h :: t when order.price = h.price && order.timestamp < h.timestamp ->
        order :: h :: t
    | h :: t -> h :: insert t
  in
  book.bids <- insert book.bids

(** Insert an ask in ascending price, ascending timestamp order *)
let insert_ask (book : t) (order : Order.t) =
  let rec insert = function
    | [] -> [ order ]
    | (h :: _) as l when order.price < h.price -> order :: l
    | h :: t when order.price = h.price && order.timestamp < h.timestamp ->
        order :: h :: t
    | h :: t -> h :: insert t
  in
  book.asks <- insert book.asks

let add_order (book : t) (order : Order.t) =
  match order.side with
  | Bid -> insert_bid book order
  | Ask -> insert_ask book order

let remove_order (book : t) (order_id : string) =
  let removed = ref None in
  let filter lst =
    List.filter
      (fun (o : Order.t) ->
        if o.id = order_id then (
          removed := Some o;
          false)
        else true)
      lst
  in
  book.bids <- filter book.bids;
  book.asks <- filter book.asks;
  !removed

let best_bid (book : t) =
  match book.bids with [] -> None | h :: _ -> Some h

let best_ask (book : t) =
  match book.asks with [] -> None | h :: _ -> Some h

let remove_best_bid (book : t) =
  match book.bids with
  | [] -> None
  | h :: t ->
      book.bids <- t;
      Some h

let remove_best_ask (book : t) =
  match book.asks with
  | [] -> None
  | h :: t ->
      book.asks <- t;
      Some h

let to_yojson (book : t) : Yojson.Safe.t =
  `Assoc
    [
      ("pair", `String (pair_key book));
      ("bids", `List (List.map Order.to_yojson book.bids));
      ("asks", `List (List.map Order.to_yojson book.asks));
    ]
