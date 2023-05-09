{ ... }: {
  perSystem = { pkgs, ... }:
    let opkgs = pkgs.ocamlPackages;
    in {
      packages.with-nixpkgs = pkgs.stdenv.mkDerivation {
        name = "morbig";
        ## NOTE: The use of `./..` matters because the path is taken as relative to
        ## the current file, and therefore to `.nix/`.
        src = ./..;

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

          ## check inputs
          alcotest
        ];

        buildPhase = ''
          make build
        '';

        installPhase = ''
          make install PREFIX=$out LIBDIR=$OCAMLFIND_DESTDIR
        '';
      };
    };
}
