{
  stdenv,
  lib,
  haskellPackages
}:

stdenv.mkDerivation {
  name = "tx-calculator";
  src = ./.;

  buildInputs = [
    (haskellPackages.ghcWithPackages (p: [
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

  meta = with lib; {
    description = "A simple Tic-Xenotation (TX) calculator";
    mainProgram = "tx";
    license = licenses.gpl3Plus;
  };
}
