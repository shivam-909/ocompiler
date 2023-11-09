# How to run this project

## Installing OCaml

MacOS:

Use Homebrew
```
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    brew install ocaml opam

    opam init
```


Windows:

https://fdopen.github.io/opam-repository-mingw/installation/

```
    opam init
```

Linux:

```
    sudo apt install ocaml opam m4
    opam init
```

## Build and run application

### Using Dune

Dune manages builds 

```
    opam install dune
```



Run these from the root of the <b>OCaml project</b> (``./ocompiler`` if you are in the repository root).

```
    dune build
    dune exec ocompiler
```

This should give you the lexer output for the 6 `.while` programs in `./data`.

## Inspecting the application

The entrypoint for the application is `./bin/main.ml`.

### Libraries

No third-party libraries are used. Three libraries are defined in the application: `io`, `lexer` and `regex`.

- `io`: Contains a single function which reads text from a file and returns the contents as a string.
- `lexer`: Contains all functionality surrounding lexing. This includes expressions that are part of the `while` language, as well as `mkeps` and `inj`.
- `regex`: Contains the basis for regular expressions in the program. Includes basic and extended regular expressions, as well as their nullability and derivatives.
