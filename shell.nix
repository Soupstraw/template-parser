{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell-language-server
    pkgs.bashInteractive
  ];
}
