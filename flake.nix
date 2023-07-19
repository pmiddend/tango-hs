# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "Haskell client interface to Tango";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    #    tango-controls.url = "git+https://gitlab.desy.de/cfel-sc-public/tango-flake?ref=v9.4.2";
    tango-controls.url = "/home/pmidden/code/tango-flake";
  };

  outputs = { self, nixpkgs, flake-utils, tango-controls }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          # pkgs = nixpkgs.legacyPackages.${system};
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ tango-controls.overlay ];
          };

          haskellPackages = pkgs.haskellPackages;

          jailbreakUnbreak = pkg:
            pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

          packageName = "tango-hs";
        in
        {
          packages.${packageName} =
            haskellPackages.callCabal2nix packageName self rec {
              # tango = pkgs.tango-controls-9_4;
              ctango = self.packages.${system}.ctango;
            };

          packages.default = self.packages.${system}.${packageName};

          packages.ctango = pkgs.stdenv.mkDerivation {
            pname = "ctango";
            version = "1.0";

            src = c_tango/.;

            nativeBuildInputs = with pkgs; [ cmake pkg-config ];
            buildInputs = with pkgs; [
              tango-controls-9_4
              zeromq
              cppzmq
              omniorb_4_2
              libjpeg_turbo
              libsodium
            ];
          };

          defaultPackage = self.packages.${system}.default;

          devShells.default =
            pkgs.mkShell {
              buildInputs = with pkgs; [
                haskellPackages.haskell-language-server # you must build it with your ghc to work
                ghcid
                haskellPackages.ormolu
                cabal-install
                haskellPackages.hlint
                haskellPackages.apply-refact

                # Needed for pkg-config tango
                tango-controls-9_4
                pkg-config
                zeromq
                cppzmq
                omniorb_4_2
                libjpeg_turbo
                libsodium
                self.packages.${system}.ctango
              ];
              # CPPZMQ_INCLUDE = "${pkgs.cppzmq}";
              # inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
              inputsFrom = [ self.packages.${system}.tango-hs.env ];
            };
          devShell = self.devShells.${system}.default;
        });
}
