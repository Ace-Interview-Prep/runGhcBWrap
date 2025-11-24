# shell.nix
let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/23.11.tar.gz";
    sha256 = "sha256:1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
  };
  pkgs = import nixpkgs {};
  haskellPackages = pkgs.haskell.packages.ghc962;
in
  haskellPackages.shellFor {
    packages = p: [ (haskellPackages.callCabal2nix "server" ./. {}) ];
    buildInputs = with pkgs; [ cabal-install haskellPackages.ghc ];
  }
