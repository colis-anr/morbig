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

      perSystem = { self', pkgs, config, ... }: {
        formatter = pkgs.nixfmt;

        packages.default = self'.packages.with-nixpkgs;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs.ocamlPackages; [ ocaml-lsp ocp-indent ];
          inputsFrom = [ self'.packages.default ];
          shellHook = config.pre-commit.installationScript;
        };

        pre-commit.settings.hooks = {
          dune-opam-sync.enable = true;
          opam-lint.enable = true;
          ocp-indent.enable = true;
          nixfmt.enable = true;
          deadnix.enable = true;

          ## NOTE: The version of the `dune-fmt` hook in `pre-commit-hooks.nix`
          ## forgets to bring OCaml in the environment. In the meantime, we use
          ## our own; will change back to `dune-fmt.enable = true` later.
          tmp-dune-fmt = {
            enable = true;
            name = "dune-fmt";
            description = "Runs Dune's formatters on the code tree.";
            entry = let
              dune-fmt = pkgs.writeShellApplication {
                name = "dune-fmt";
                text = ''
                  export PATH=${pkgs.ocaml}/bin:$PATH
                  exec ${pkgs.dune_3}/bin/dune fmt "$@"
                '';
              };
            in "${dune-fmt}/bin/dune-fmt";
            pass_filenames = false;
          };
        };
      };

      ## Improve the way `inputs'` are computed by also handling the case of
      ## flakes having a `lib.${system}` attribute (eg. `opam-nix`).
      perInput = system: flake:
        if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };
    };
}
