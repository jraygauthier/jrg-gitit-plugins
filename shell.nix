{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, directory
      , filepath, filestore, gitit, hslogger, mtl, network, network-uri
      , pandoc, pandoc-types, process, SHA, stdenv, utf8-string
      }:
      mkDerivation {
        pname = "jrg-gitit-plugins";
        version = "0.0.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring directory filepath filestore gitit
          hslogger mtl network network-uri pandoc pandoc-types process SHA
          utf8-string
        ];
        license = "GPL";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
