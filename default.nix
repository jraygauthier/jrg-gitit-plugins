{ mkDerivation, base, bytestring, datetime, directory, filepath
, filestore, gitit, hslogger, mtl, network, network-uri, pandoc
, pandoc-types, process, SHA, stdenv, utf8-string
}:
mkDerivation {
  pname = "jrg-gitit-plugins";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring datetime directory filepath filestore gitit
    hslogger mtl network network-uri pandoc pandoc-types process SHA
    utf8-string
  ];
  license = "GPL";
}
