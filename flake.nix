{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

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
    {
      self,
      nixpkgs,
      git-hooks,
      topiary,
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      opkgs = pkgs.ocamlPackages;

      inherit (topiary.lib.${system}) gitHookBinFor gitHookFor;
      myTopiaryConfig = {
        includeLanguages = [
          "ocaml"
          "ocaml_interface"
        ];
      };

    in
    {
      packages.${system}.default = opkgs.buildDunePackage {
        pname = "monadise";
        version = "dev";
        src = ./.;

        doCheck = true;
        checkInputs = with opkgs; [ alcotest ];
      };

      devShells.${system}.default = pkgs.mkShell {
        inputsFrom = [ self.packages.${system}.default ];
        inherit (self.checks.${system}.git-hooks) shellHook;
        buildInputs = self.checks.${system}.git-hooks.enabledPackages ++ [
          (gitHookBinFor myTopiaryConfig)
        ];
      };

      checks.${system}.git-hooks = git-hooks.lib.${system}.run {
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
