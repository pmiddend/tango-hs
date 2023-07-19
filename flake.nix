# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "Haskell client interface to Tango";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    tango-controls.url = "git+https://gitlab.desy.de/cfel-sc-public/tango-flake?ref=v9.4.2";
  };

  outputs = { self, nixpkgs, flake-utils, tango-controls }:
    flake-utils.lib.eachDefaultSystem (system:
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
            # Dependency overrides go here
          };

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
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
            omniorb_4_2
            libjpeg_turbo
          ];
          CPPZMQ_INCLUDE = "${pkgs.cppzmq}/include";
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
