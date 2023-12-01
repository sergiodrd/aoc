{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs.ocamlPackages; [
    ocaml
    utop
    dune_3
    core
  ];
}
