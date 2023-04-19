{ self, ... }: {
  perSystem = { inputs', pkgs, ... }:
    let
      scope =
        inputs'.opam-nix.lib.buildOpamProject { inherit pkgs; } "morbig" self {
          ocaml-base-compiler = "*";
        };
    in { packages.with-opam-nix = scope.morbig // { inherit scope; }; };
}
