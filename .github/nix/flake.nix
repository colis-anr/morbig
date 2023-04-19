{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      imports = [
        ./with-nixpkgs.nix
        ./with-opam-nix.nix
        inputs.pre-commit-hooks.flakeModule
      ];

      perSystem = { self', pkgs, ... }: {
        formatter = pkgs.nixfmt;

        packages.default = self'.packages.with-nixpkgs;

        pre-commit.settings.hooks = {
          dune-opam-sync.enable = true;
          opam-lint.enable = true;
          ocp-indent.enable = true;
          nixfmt.enable = true;
          deadnix.enable = true;
        };
      };

      ## Improve the way `inputs'` are computed by also handling the case of
      ## flakes having a `lib.${system}` attribute (eg. `opam-nix`).
      perInput = system: flake:
        if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };
    };
}
