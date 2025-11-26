{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix;
    # { mkDerivation, base, directory, filepath, lib, process
    #   , temporary, text, which
    #   }:
    #   mkDerivation {
    #     pname = "runGhcBWrap";
    #     version = "0.1.0.0";
    #     src = ./.;
    #     libraryHaskellDepends = [
    #       base directory filepath process temporary text which
    #     ];
    #     license = "unknown";
    #   };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
