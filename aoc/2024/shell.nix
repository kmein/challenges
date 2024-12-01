{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [
    pkgs.lua

    (pkgs.writers.writeDashBin "which-language" ''
      ${pkgs.gnugrep}/bin/grep '^- \[ ]' README.md | ${pkgs.coreutils}/bin/shuf -n 1
    '')
  ];
}
