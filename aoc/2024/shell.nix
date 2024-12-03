{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [
    pkgs.lua #1
    pkgs.gnat #2
    pkgs.fpc #3

    (pkgs.writers.writeDashBin "which-language" ''
      ${pkgs.gnugrep}/bin/grep '^- \[ ]' README.md | ${pkgs.coreutils}/bin/shuf -n 1
    '')
  ];
}
