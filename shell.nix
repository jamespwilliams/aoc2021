with import <nixpkgs> {};

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_12;
in

pkgs.mkShell {
   buildInputs = with ocamlPackages; [
    ocaml
    findlib
    dune_2
    ocaml-lsp
    ocamlformat
    core
    utop
    ppx_jane
  ];
}
