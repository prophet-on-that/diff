let
  nixpkgs = import <nixpkgs> {};

  inherit (nixpkgs) stdenv;
in
  stdenv.mkDerivation {
     name = "diff-env";
     buildInputs = [
       nixpkgs.stack
     ];
  }
