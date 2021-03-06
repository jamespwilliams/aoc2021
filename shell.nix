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
    re2
    core
    utop
    ppx_deriving
    ppx_jane
  ];
}
