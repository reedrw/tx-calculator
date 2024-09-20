{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    genSystems = nixpkgs.lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
    pkgsFor = system: nixpkgs.legacyPackages."${system}";
  in {
    devShells = genSystems (system: let pkgs = pkgsFor system; in {
      default = pkgs.mkShell {
        name = "tx-calculator";
        packages = with pkgs; [
          (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
            haskellPackages.data-ordlist
          ]))
        ];
      };
    });

    packages = genSystems (system: let pkgs = pkgsFor system; in {
      tx-calculator = pkgs.stdenv.mkDerivation {
        name = "tx-calculator";
        src = ./.;
        buildInputs = with pkgs; [
          (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
            haskellPackages.data-ordlist
          ]))
        ];
        buildPhase = ''
          ghc ./tx.hs -o tx
        '';
        installPhase = ''
          mkdir -p $out/bin
          cp ./tx $out/bin
        '';
      };
    });
  };
}
