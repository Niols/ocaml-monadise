{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    topiary = {
      url = "github:tweag/topiary";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      perSystem =
        {
          self',
          pkgs,
          inputs',
          ...
        }:
        let
          opkgs = pkgs.ocamlPackages;

          inherit (inputs'.topiary.lib) gitHookBinFor gitHookFor;
          myTopiaryConfig = {
            includeLanguages = [
              "ocaml"
              "ocaml_interface"
            ];
          };

        in
        {
          packages.monadise = opkgs.buildDunePackage {
            pname = "monadise";
            version = "dev";
            src = ./.;
            doCheck = true;
            checkInputs = with opkgs; [ alcotest ];
          };

          packages.monadise-lwt = opkgs.buildDunePackage {
            pname = "monadise-lwt";
            version = "dev";
            src = ./.;
            buildInputs = [ self'.packages.monadise ] ++ (with opkgs; [ lwt ]);
          };

          devShells.default = pkgs.mkShell {
            inputsFrom = with self'.packages; [
              monadise
              monadise-lwt
            ];
            inherit (self'.checks.git-hooks) shellHook;
            buildInputs = self'.checks.git-hooks.enabledPackages ++ [
              (gitHookBinFor myTopiaryConfig)
            ];
          };

          checks.git-hooks = inputs'.git-hooks.lib.run {
            src = ./.;
            hooks = {
              deadnix.enable = true;
              dune-fmt.enable = true;
              dune-opam-sync.enable = true;
              nixfmt-rfc-style.enable = true;
              topiary-latest = gitHookFor myTopiaryConfig // {
                enable = true;
              };
            };
          };
        };

      ## Improve the way `inputs'` are computed by also handling the case of
      ## flakes having a `lib.${system}` attribute.
      ##
      perInput = system: flake: if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };
    };

  nixConfig = {
    extra-trusted-substituters = [
      "https://pre-commit-hooks.cachix.org/"
      "https://tweag-topiary.cachix.org/"
    ];
    extra-trusted-public-keys = [
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "tweag-topiary.cachix.org-1:8TKqya43LAfj4qNHnljLpuBnxAY/YwEBfzo3kzXxNY0="
    ];
  };
}
