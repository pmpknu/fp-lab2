(lang dune 3.16)

(name bt_bag)

(generate_opam_files true)

(source
 (github pmpknu/fp-lab2))

(authors "Dan Gorlyakov")

(maintainers "Dan Gorlyakov")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name bt_bag)
 (synopsis "Functional Programming lab2")
 (description "implementation of bag with binary tree")
 (depends ocaml dune (alcotest :with-test) qcheck qcheck-alcotest ppx_expect ocamlformat zanuda)
)