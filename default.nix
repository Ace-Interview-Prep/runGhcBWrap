{ pkgs, base, data-default, lens, lib, template-haskell, which
, text, directory, filepath, temporary, process, mkDerivation
}:
let
  pkgs_unstable = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
  }) {};
  ghc_unstable = pkgs_unstable.haskell.packages.ghc912;
  ghc_9_12 = ghc_unstable.ghcWithPackages (
    hpkgs: with hpkgs; [
      temporary vector aeson parsec
    ]
  );
in
mkDerivation {
  pname = "runGhcBWrap";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base data-default lens template-haskell text which
    directory filepath temporary process
  ];
  librarySystemDepends = [
    ghc_9_12
    pkgs.bubblewrap
    pkgs.nix
  ];
  homepage = "https://github.com/augyg/ClasshSS";
  description = "Typified Tailwind for Rapid Development";
  license = lib.licenses.mit;
}
