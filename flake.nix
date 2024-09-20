{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in {
    devShells.x86_64-linux.default = pkgs.mkShell {
      name = "tx-calculator";
      buildInputs = with pkgs; [
        (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
          haskellPackages.data-ordlist
        ]))
      ];
    };
    packages.x86_64-linux.tx-calculator = pkgs.stdenv.mkDerivation {
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
  };
}
