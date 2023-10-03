(lang dune 2.2)
(name odate)
(using menhir 2.0)

(generate_opam_files true)
(authors "Hugo Heuzard")
(maintainers "Hugo Heuzard <hugo.heuzard@gmail.com>")
(source (github hhugo/odate))
(homepage "https://github.com/hhugo/odate")
(documentation "https://github.com/hhugo/odate")
(license "MIT")

(package
 (name odate)
 (depends
  (ocaml (>= 4.07))
  (menhir (>= 20230608))
  (ppx_expect :with-test)
 )
 (description
"Simple date and duration manipulation. Also implement duration printer
based on string format. Already implemented in opalang
[http://opalang.org/]. For documentation about the format, see :
[http://doc.opalang.org/value/stdlib.core.date/Duration/try_generate_printer].")
 (synopsis "Date & Duration Library")
)