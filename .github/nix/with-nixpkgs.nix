{ ... }: {
  perSystem = { pkgs, ... }:
    let opkgs = pkgs.ocamlPackages;
    in {
      packages.with-nixpkgs = opkgs.buildDunePackage rec {
        pname = "morbig";
        version = "dev";
        src = ./.;

        duneVersion = "3";

        nativeBuildInputs = with opkgs; [ menhir ];
        propagatedBuildInputs = with opkgs; [
          menhirLib
          ppx_deriving_yojson
          visitors
          yojson
        ];
      };
    };
}
