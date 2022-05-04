{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, filepath, hspec, lib, mtl
      , optparse-applicative, parsec, raw-strings-qq, tasty, tasty-hspec
      }:
      mkDerivation {
        pname = "wyascheme";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [ base filepath mtl parsec ];
        executableHaskellDepends = [ base mtl optparse-applicative ];
        testHaskellDepends = [
          base hspec mtl parsec raw-strings-qq tasty tasty-hspec
        ];
        doHaddock = false;
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
