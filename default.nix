{ mkDerivation, base, containers, stdenv, text, vector }:
mkDerivation {
  pname = "Advent";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers text vector ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
