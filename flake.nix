{
  description = "A simple Tic-Xenotation (TX) calculator";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = inputs@{ flake-parts, ... }: flake-parts.lib.mkFlake { inherit inputs; }
  {
    systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
    perSystem = { config, self', inputs', pkgs, system, ... }: {
      devShells.default = pkgs.mkShell {
        name = "tx-calculator";
        packages = with pkgs; [
          (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
            haskellPackages.arithmoi
          ]))
        ];
      };

      packages = {
        default = self'.packages.tx-calculator;
        tx-calculator = pkgs.callPackage ./package.nix { };
      };
    };
  };
}
