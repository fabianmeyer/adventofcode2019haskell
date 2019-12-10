{ mkDerivation, base, containers, hpack, stdenv, text, vector }:
mkDerivation {
  pname = "Advent";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers text vector ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base containers text vector ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
