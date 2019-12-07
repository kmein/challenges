{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [ chicken rlwrap ];
  shellHook = ''
    export HISTFILE=${toString ./.history}
    alias csi="rlwrap csi"
  '';
}
