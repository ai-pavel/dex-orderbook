(** Settlement module: tracks per-trader per-token balances with atomic updates. *)

type balances = (string, (string, float) Hashtbl.t) Hashtbl.t

let create () : balances = Hashtbl.create 16

let get_balance (balances : balances) ~trader ~token =
  match Hashtbl.find_opt balances trader with
  | None -> 0.0
  | Some tokens -> (
      match Hashtbl.find_opt tokens token with None -> 0.0 | Some b -> b)

let set_balance (balances : balances) ~trader ~token ~amount =
  let tokens =
    match Hashtbl.find_opt balances trader with
    | Some t -> t
    | None ->
        let t = Hashtbl.create 8 in
        Hashtbl.replace balances trader t;
        t
  in
  Hashtbl.replace tokens token amount

let deposit (balances : balances) ~trader ~token ~amount =
  let current = get_balance balances ~trader ~token in
  set_balance balances ~trader ~token ~amount:(current +. amount)

let has_sufficient_balance (balances : balances) ~trader ~token ~amount =
  get_balance balances ~trader ~token >= amount -. 1e-10

(** Atomically settle a trade: transfer base from seller to buyer,
    and quote from buyer to seller. Returns true if successful. *)
let settle_trade (balances : balances) ~buyer ~seller ~base_token ~quote_token
    ~quantity ~price =
  let quote_amount = quantity *. price in
  let buyer_quote = get_balance balances ~trader:buyer ~token:quote_token in
  let seller_base = get_balance balances ~trader:seller ~token:base_token in
  (* Check both sides have sufficient balance *)
  if buyer_quote >= quote_amount -. 1e-10 && seller_base >= quantity -. 1e-10
  then (
    (* Atomic update: debit buyer's quote, credit buyer's base *)
    set_balance balances ~trader:buyer ~token:quote_token
      ~amount:(buyer_quote -. quote_amount);
    deposit balances ~trader:buyer ~token:base_token ~amount:quantity;
    (* Debit seller's base, credit seller's quote *)
    set_balance balances ~trader:seller ~token:base_token
      ~amount:(seller_base -. quantity);
    deposit balances ~trader:seller ~token:quote_token ~amount:quote_amount;
    true)
  else false

let trader_balances_to_yojson (balances : balances) ~trader : Yojson.Safe.t =
  match Hashtbl.find_opt balances trader with
  | None -> `Assoc [ ("trader", `String trader); ("balances", `Assoc []) ]
  | Some tokens ->
      let entries =
        Hashtbl.fold
          (fun token amount acc -> (token, `Float amount) :: acc)
          tokens []
      in
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) entries in
      `Assoc [ ("trader", `String trader); ("balances", `Assoc sorted) ]
