# aoc2021

Disclaimer: there's a 99% chance I'll give up on this after a few days, but
anyway, here are some OCaml solutions.

## Running the code

In the `ocaml` directory run:

```
dune exec mainA.exe
dune exec mainB.exe
```

## Development

utop is useful when developing:

```
dune utop
```

Then in `utop` you can run e.g:

```
#use "mainA.ml";;
```

To load `mainA.ml`. You can then interact with the functions and values in
`mainA`.
