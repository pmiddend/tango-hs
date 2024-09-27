# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "Haskell client interface to Tango";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    tango-controls.url = "git+https://gitlab.desy.de/cfel-sc-public/tango-flake";
  };

  outputs = { self, nixpkgs, flake-utils, tango-controls }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ tango-controls.overlays.default ];
          };

          haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: {
              log-base = pkgs.haskell.lib.markUnbroken super.log-base;
            };
          };

          packageName = "hs-tango";

          # my-cpptango = pkgs.tango-cpptango.overrideAttrs (old: {
          #   src = /home/pmidden/code/tango-projects/cppTango;
          # });
          my-cpptango = pkgs.cpptango-9_4;
        in
        {
          packages.${packageName} =
            haskellPackages.callCabal2nix packageName self {
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
              my-cpptango
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

                # debugging
                valgrind
                gdb

                # Needed for pkg-config tango
                my-cpptango
                clang-tools
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
              inputsFrom = [ self.packages.${system}.hs-tango.env ];
            };
          devShell = self.devShells.${system}.default;
        });
}
