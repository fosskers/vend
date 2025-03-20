{
  description = "Vend";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem
    (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        src = ./.;
      in {
        devShell = pkgs.mkShell {
          nativeBuildInputs = [];
          buildInputs = [ pkgs.ecl ];
        };
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "vend";
          version = "0.1.5";

          src = src;

          nativeBuildInputs = [];
          buildInputs = [ pkgs.ecl ];

          buildPhase = ''
            export HOME=$(pwd)
            ${pkgs.ecl}/bin/ecl --load build.lisp
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp -v vend $out/bin
          '';
        };
      }
    );
}
