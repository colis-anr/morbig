{ ... }: {
  perSystem = { inputs', pkgs, ... }:
    ## NOTE: The use of `./..` matters because the path is taken as relative to
    ## the current file, and therefore to `.nix/`.
    let
      scope =
        inputs'.opam-nix.lib.buildOpamProject { inherit pkgs; } "morbig" ./.. {
          ocaml-base-compiler = "*";
        };
    in { packages.with-opam-nix = scope.morbig // { inherit scope; }; };
}
