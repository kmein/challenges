{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [
    pkgs.lua #1
    pkgs.gnat #2
    pkgs.fpc #3
    pkgs.nim #4
    pkgs.deno #5
    pkgs.go #6
    # pkgs.gmp pkgs.R pkgs.rstudio #7
    pkgs.rustc #8
    pkgs.scala #9
    pkgs.perl #10
    pkgs.php #11
    pkgs.chicken #12
    pkgs.dart #13
    pkgs.gcc #14
    pkgs.groovy #15
    pkgs.openjdk #16
    pkgs.dotnet-sdk #17
    pkgs.python3Packages.networkx pkgs.bash-language-server #18
    pkgs.ruby #19
    pkgs.ocaml #22
    pkgs.kotlin #23
    (pkgs.ghc.withPackages (hs: [hs.containers hs.text])) #24

    (pkgs.writers.writeDashBin "which-language" ''
      ${pkgs.gnugrep}/bin/grep '^- \[ ]' README.md | ${pkgs.coreutils}/bin/shuf -n 1
    '')
  ];
}
