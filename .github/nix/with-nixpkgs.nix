{ self, ... }: {
  perSystem = { pkgs, ... }:
    let opkgs = pkgs.ocamlPackages;
    in {
      packages.with-nixpkgs = pkgs.stdenv.mkDerivation {
        name = "morbig";
        src = self;

        nativeBuildInputs = with opkgs; [
          ## Basic ones, always necessary
          ocaml
          dune_3
          findlib
          ## Specific to our project
          menhir
        ];

        buildInputs = with opkgs; [
          menhirLib
          ppx_deriving_yojson
          visitors
          yojson
        ];

        buildPhase = ''
          pwd
          ls
          cat Makefile
          make build
        '';

        installPhase = ''
          make install PREFIX=$out
        '';
      };
    };
}
