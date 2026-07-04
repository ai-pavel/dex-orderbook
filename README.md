# DEX Order Book Engine

[![CI](https://github.com/pavel-genai/dex-orderbook/actions/workflows/ci.yml/badge.svg)](https://github.com/pavel-genai/dex-orderbook/actions/workflows/ci.yml)

An OCaml implementation of a decentralized exchange (DEX) order book engine with price-time priority matching, partial fills, and atomic settlement.

## Features

- Limit orders with price, quantity, side (bid/ask), timestamp, and trader address
- Matching engine using priority queues (max-heap for bids, min-heap for asks)
- Price-time priority matching with partial fill support
- Cancel and replace order support
- Per-trader per-token balance tracking with atomic settlement
- Self-trade prevention
- Insufficient balance rejection
- JSON command-line interface (stdin/stdout)

## Project Structure

```
lib/
  order.ml          - Order types and serialization
  orderbook.ml      - Order book with bid/ask priority queues
  matching_engine.ml - Matching engine with price-time priority
  settlement.ml     - Balance tracking and atomic settlement
bin/
  main.ml           - CLI reading JSON commands from stdin
test/
  test_engine.ml    - Alcotest tests
```

## Building

```bash
opam install . --deps-only
dune build
```

## Running

```bash
dune exec dex_orderbook
```

Then send newline-delimited JSON commands on stdin:

```json
{"command": "place_order", "id": "1", "trader": "alice", "side": "bid", "price": 100.0, "quantity": 10.0, "base": "ETH", "quote": "USDC"}
{"command": "get_book", "base": "ETH", "quote": "USDC"}
{"command": "get_balances", "trader": "alice"}
{"command": "cancel_order", "id": "1", "base": "ETH", "quote": "USDC"}
```

## Testing

```bash
dune runtest
```
