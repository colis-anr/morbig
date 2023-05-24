(lang dune 2.5)

(name morbig)
(source (github colis-anr/morbig))

(maintainers "Nicolas “Niols” Jeannerod <niols@niols.fr>")
(authors
  "Yann Régis-Gianas <yann.regis-gianas@irif.fr>"
  "Ralf Treinen <ralf.treinen@irif.fr>"
  "Nicolas Jeannerod <niols@niols.fr>")
(license GPL3-only)

(package
 (name morbig)
 (version 0.11.0)

 (synopsis "A trustworthy parser for POSIX shell")
 (description
  "\| Morbig is a parser for shell scripts written in the POSIX shell script
  "\| language. It parses the scripts statically, that is without executing
  "\| them, and constructs a concrete syntax tree for each of them. The
  "\| concrete syntax trees are built using constructors according to the
  "\| shell grammar of the POSIX standard.
  )

 (depends
  (conf-jq              :with-test)
  (alcotest             (and :with-test (>= "1.7.0")))
  (menhir               (>= "20200211"))
  (ocaml                (>= "4.04"))
  (odoc                 :with-doc)
   ppx_deriving_yojson
  (qcheck               (and :with-test (>= "0.18")))
  (qcheck-alcotest      (and :with-test (>= "0.18")))
  (visitors             (>= "20200207"))
  (yojson               (>= "1.6.0"))))

(generate_opam_files true)
(using menhir 2.0)
(formatting (enabled_for dune))
