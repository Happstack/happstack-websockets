{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, ghcjs-base, stdenv }:
      mkDerivation {
        pname = "happstack-websockets-example-client";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base ghcjs-base bytestring ];
        buildTools = [ pkgs.haskellPackages.cabal-install pkgs.haskellPackages.ghc ];
        description = "An example of using ghcjs-websockets with happstack-websockets";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
