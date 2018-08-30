{ mkDerivation, base, containers, stdenv }:
mkDerivation {
  pname = "prob";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers ];
  license = stdenv.lib.licenses.bsd3;
}
