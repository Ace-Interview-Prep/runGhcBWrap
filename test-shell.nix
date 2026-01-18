{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:
let
  inherit (nixpkgs) pkgs;
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
  
  f = import ./default.nix;

  haskellPackages' = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  # Override the package set to include runGhcBWrap-core
  haskellPackages = haskellPackages'.override {
    overrides = self: super: {
      runGhcBWrap-core = super.callCabal2nix "runGhcBWrap-core" sources.runGhcBWrap-core {};
      IStr = IStr_;
      scrappy-core = scrappy-core_;
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callPackage f {});
in
if pkgs.lib.inNixShell then drv.env else drv
