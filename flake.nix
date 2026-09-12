{
  description = "Oaks Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        ocamlPackages = pkgs.ocamlPackages;
      in
      {
        packages.default = ocamlPackages.buildDunePackage {
          pname = "oaks";
          version = "0.1.0";

          src = ./.;

          duneVersion = "3";

          # Opam packages needed
          propagatedBuildInputs = with ocamlPackages; [
            dns
            lwt
            mirage-crypto
            mirage-crypto-rng
          ];

          # Libraries needed at compile-time
          nativeBuildInputs = [
            pkgs.pkg-config
          ];

          # Libraries needed at runtime
          buildInputs = [
          ];
        };

        devShells.default = pkgs.mkShell {
          packages = [
            ocamlPackages.ocaml
            ocamlPackages.dune_3
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
            ocamlPackages.utop
            ocamlPackages.findlib

            ocamlPackages.dns
            ocamlPackages.lwt
            ocamlPackages.mirage-crypto
            ocamlPackages.mirage-crypto-rng
          ];
        };
      }
    );
}
