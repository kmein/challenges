{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [
    pkgs.lua #1
    pkgs.gnat #2
    pkgs.fpc #3
    pkgs.nim #4
    pkgs.deno #5
    pkgs.go #6
    pkgs.gmp pkgs.R pkgs.rstudio #7
    pkgs.rustc #8

    (pkgs.writers.writeDashBin "which-language" ''
      ${pkgs.gnugrep}/bin/grep '^- \[ ]' README.md | ${pkgs.coreutils}/bin/shuf -n 1
    '')
  ];
}
