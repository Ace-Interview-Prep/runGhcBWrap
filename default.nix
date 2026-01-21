{ pkgs, base, data-default, lens, lib, template-haskell, which
, text, directory, filepath, temporary, process, runGhcBWrap-core, mkDerivation
}:
let
  pkgs_unstable = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
  }) {};
  #inherit (nixpkgs) pkgs;
  n_ = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "nix-thunk";
    rev = "8fe6f2de2579ea3f17df2127f6b9f49db1be189f";
    sha256 = "14l2k6wipam33696v3dr3chysxhqcy0j7hxfr10c0bxd1pxv7s8b";
  };
  n = import n_ {};
  sources = n.mapSubdirectories n.thunkSource ./thunks;
  runGhcBWrap-core_ = pkgs.haskellPackages.callCabal2nix "runGhcBWrap-core" sources.runGhcBWrap-core {};
  IStr_ = pkgs.haskellPackages.callCabal2nix "IStr" sources.IStr {};
  scrappy-core_ = pkgs.haskellPackages.callCabal2nix "scrappy-core" sources.scrappy-core {};

  ghc_unstable = pkgs_unstable.haskell.packages.ghc912;

  overrides_ = pre: post: {
    runGhcBWrap-core = post.callCabal2nix "runGhcBWrap-core" sources.runGhcBWrap-core {};
    IStr = pre.callCabal2nix "IStr" sources.IStr {};
    scrappy-core = pre.callCabal2nix "scrappy-core" sources.scrappy-core {};
  };
  
  ghc_9_12 = (pkgs.haskell.packages.ghc912.override { overrides = overrides_; }).ghcWithPackages (
    hpkgs: with hpkgs; [
      temporary vector aeson parsec hpkgs.runGhcBWrap-core hpkgs.IStr hpkgs.scrappy-core
    ]
  );
  # ghc_9_12 = ghc_unstable.ghcWithPackages (
  #   hpkgs: with hpkgs; [
  #     temporary vector aeson parsec runGhcBWrap-core_ IStr_ scrappy-core_
  #   ]
  # );
in
mkDerivation {
  pname = "runGhcBWrap";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base data-default lens template-haskell text which
    directory filepath temporary process runGhcBWrap-core
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
