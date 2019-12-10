{ mkDerivation, base, containers, stdenv, text, vector }:
mkDerivation {
  pname = "Advent";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers text vector ];
  executableHaskellDepends = [ base containers text vector ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
