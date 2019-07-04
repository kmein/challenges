{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.exercism
    pkgs.stack
  ];
  shellHook = ''
    ${pkgs.exercism}/bin/exercism configure --workspace $PWD/exercism
  '';
}
