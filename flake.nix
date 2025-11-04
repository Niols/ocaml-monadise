{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      opkgs = pkgs.ocamlPackages;
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
      };
    };
}
