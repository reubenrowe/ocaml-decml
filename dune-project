(lang dune 1.11)
(name decml)
(explicit_js_mode)
(authors
  "Reuben N. S. Rowe <reuben.rowe@cantab.net>"
  "Victor Darvariu <victor.darvariu@gmail.com>")
(maintainers
  "Reuben N. S. Rowe <reuben.rowe@cantab.net>")
(source (github reubenrowe/ocaml-decml))
(bug_reports "https://github.com/reubenrowe/ocaml-decml/issues")
(homepage "https://github.com/reubenrowe/ocaml-decml")

(package
 (name decml)
 (synopsis "A PPX rewriter and library for abductive programming")
 (description
  "A PPX rewriter and library for abductive programming.")
 (depends
  (containers (>= 3.0))
  (csv :build)
  (dune (>= 1.0))
  (ocaml (>= 4.04))
  (ocaml-migrate-parsetree (< 2.0.0))))

(generate_opam_files true)
