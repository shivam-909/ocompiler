(lang dune 3.7)

(name ocompiler)

(generate_opam_files true)

(source
 (github shivam-909/ocompiler))

(authors "Shivam Acharya")

(maintainers "Shivam Acharya")

(license MIT)

(map_workspace_root false)


(package
 (name ocompiler)
 (synopsis "A compiler for the WHILE language targeting the Jasmin assembler.")
 (description "A compiler for the WHILE language targetting the Jasmin assembler.")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
