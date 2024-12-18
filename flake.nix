{
  description = "A simple Tic-Xenotation (TX) calculator";

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
            haskellPackages.arithmoi
          ]))
        ];
      };
    });

    packages = genSystems (system: let pkgs = pkgsFor system; in rec {
      default = tx-calculator;
      tx-calculator = pkgs.stdenv.mkDerivation {
        name = "tx-calculator";
        src = ./.;
        buildInputs = with pkgs; [
          (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
            haskellPackages.arithmoi
          ]))
        ];
        buildPhase = ''
          ghc ./tx.hs -o tx
        '';
        installPhase = ''
          mkdir -p $out/bin
          cp ./tx $out/bin
        '';
        meta = with pkgs.lib; {
          description = "A simple Tic-Xenotation (TX) calculator";
          mainProgram = "tx";
          license = licenses.gpl3Plus;
        };
      };
    });
  };
}
