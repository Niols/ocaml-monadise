(lang dune 3.20)

(name monadise)

(source (github niols/monadise))

(authors "Niols <niols@niols.fr>")
(maintainers "Niols <niols@niols.fr>")

(generate_opam_files true)

(package
 (name monadise)
 (synopsis "Monadise direct-style functions easily")
 (depends
  (ocaml (>= 5.0))
  (alcotest :with-test)
  (alcotest-lwt :with-test)
  (lwt :with-test)))
