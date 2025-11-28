# default.nix
let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/23.11.tar.gz";
    sha256 = "sha256:1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
  };

  pkgs = import nixpkgs {
    config = {
      allowUnfree = true;
    };
  };

  haskellPackages = pkgs.haskell.packages.ghc962.override {
    overrides = self: super: {
      # Add any custom overrides here if needed
    };
  };

  server = haskellPackages.callCabal2nix "server" ./. {};
in
  # Enable caching and build the derivation
  pkgs.stdenv.mkDerivation {
    name = "ace-runGHC-server";
    src = ./.;
    buildInputs = [ server pkgs.cachix pkgs.curl pkgs.jq ];
    installPhase = ''
      mkdir -p $out/bin
      cp ${server}/bin/server $out/bin/server
    '';
    preferLocalBuild = true;
    allowSubstitutes = true;
  }
