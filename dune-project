(lang dune 3.0)

(name dex_orderbook)

(generate_opam_files true)

(source
 (github pavel-genai/dex-orderbook))

(authors "Pavel")

(license MIT)

(package
 (name dex_orderbook)
 (synopsis "Decentralized exchange order book engine")
 (description "An OCaml implementation of a DEX order book with matching engine, settlement, and CLI")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (yojson (>= 2.0))
  (alcotest (and (>= 1.6) :with-test))
  (bisect_ppx (and (>= 2.6) :with-test))))

(instrumentation (backend bisect_ppx))
