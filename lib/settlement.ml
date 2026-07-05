(** Settlement module: tracks per-trader per-token balances with atomic updates.

    In addition to on-hand balances, we track [reserved] funds: amounts that
    have been locked to back resting orders on the book. The [available]
    balance a trader can commit to a new order is [balance - reserved]. *)

type book = (string, (string, float) Hashtbl.t) Hashtbl.t

type balances = { balances : book; reserved : book }

let create () : balances =
  { balances = Hashtbl.create 16; reserved = Hashtbl.create 16 }

let get_from (b : book) ~trader ~token =
  match Hashtbl.find_opt b trader with
  | None -> 0.0
  | Some tokens -> (
      match Hashtbl.find_opt tokens token with None -> 0.0 | Some v -> v)

let set_in (b : book) ~trader ~token ~amount =
  let tokens =
    match Hashtbl.find_opt b trader with
    | Some t -> t
    | None ->
        let t = Hashtbl.create 8 in
        Hashtbl.replace b trader t;
        t
  in
  Hashtbl.replace tokens token amount

let get_balance (bals : balances) ~trader ~token =
  get_from bals.balances ~trader ~token

let set_balance (bals : balances) ~trader ~token ~amount =
  set_in bals.balances ~trader ~token ~amount

let get_reserved (bals : balances) ~trader ~token =
  get_from bals.reserved ~trader ~token

(** Available = on-hand balance minus funds reserved for resting orders. *)
let get_available (bals : balances) ~trader ~token =
  get_balance bals ~trader ~token -. get_reserved bals ~trader ~token

(** Lock [amount] of [token] for [trader] to back a resting order. *)
let lock (bals : balances) ~trader ~token ~amount =
  let current = get_reserved bals ~trader ~token in
  set_in bals.reserved ~trader ~token ~amount:(current +. amount)

(** Release [amount] of previously reserved [token] for [trader]. *)
let unlock (bals : balances) ~trader ~token ~amount =
  let current = get_reserved bals ~trader ~token in
  let next = Float.max 0.0 (current -. amount) in
  set_in bals.reserved ~trader ~token ~amount:next

let deposit (bals : balances) ~trader ~token ~amount =
  let current = get_balance bals ~trader ~token in
  set_balance bals ~trader ~token ~amount:(current +. amount)

(** True if the trader's [available] (unreserved) balance covers [amount]. *)
let has_sufficient_balance (bals : balances) ~trader ~token ~amount =
  get_available bals ~trader ~token >= amount -. 1e-10

(** Atomically settle a trade: transfer base from seller to buyer,
    and quote from buyer to seller. Returns true if successful. *)
let settle_trade (bals : balances) ~buyer ~seller ~base_token ~quote_token
    ~quantity ~price =
  let quote_amount = quantity *. price in
  let buyer_quote = get_balance bals ~trader:buyer ~token:quote_token in
  let seller_base = get_balance bals ~trader:seller ~token:base_token in
  (* Check both sides have sufficient balance *)
  if buyer_quote >= quote_amount -. 1e-10 && seller_base >= quantity -. 1e-10
  then (
    (* Atomic update: debit buyer's quote, credit buyer's base *)
    set_balance bals ~trader:buyer ~token:quote_token
      ~amount:(buyer_quote -. quote_amount);
    deposit bals ~trader:buyer ~token:base_token ~amount:quantity;
    (* Debit seller's base, credit seller's quote *)
    set_balance bals ~trader:seller ~token:base_token
      ~amount:(seller_base -. quantity);
    deposit bals ~trader:seller ~token:quote_token ~amount:quote_amount;
    true)
  else false

let trader_balances_to_yojson (bals : balances) ~trader : Yojson.Safe.t =
  match Hashtbl.find_opt bals.balances trader with
  | None -> `Assoc [ ("trader", `String trader); ("balances", `Assoc []) ]
  | Some tokens ->
      let entries =
        Hashtbl.fold
          (fun token amount acc -> (token, `Float amount) :: acc)
          tokens []
      in
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) entries in
      `Assoc [ ("trader", `String trader); ("balances", `Assoc sorted) ]
