{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      imports = [
        .nix/with-nixpkgs.nix
        .nix/with-opam-nix.nix
        inputs.pre-commit-hooks.flakeModule
      ];

      perSystem = { self', pkgs, config, ... }: {
        formatter = pkgs.nixfmt;

        packages.default = self'.packages.with-nixpkgs;

        devShells.default = pkgs.mkShell {
          buildInputs = (with pkgs; [ headache ])
            ++ (with pkgs.ocamlPackages; [ ocaml-lsp ocp-indent ]);
          inputsFrom = [ self'.packages.default ];
          shellHook = config.pre-commit.installationScript;
        };

        pre-commit.settings.hooks = {
          dune-opam-sync.enable = true;
          opam-lint.enable = true;
          ocp-indent.enable = true;
          nixfmt.enable = true;
          deadnix.enable = true;
          dune-fmt.enable = true;
        };
      };

      ## Improve the way `inputs'` are computed by also handling the case of
      ## flakes having a `lib.${system}` attribute (eg. `opam-nix`).
      perInput = system: flake:
        if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };
    };
}
