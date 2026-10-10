{ mkDerivation, lib, array, base, containers, HUnit, haskell-src-exts
, QuickCheck, transformers
}:
mkDerivation {
  pname = "pointfree";
  version = "1.1.1.12";
  src = ./.;
  libraryHaskellDepends = [
    array base containers haskell-src-exts transformers
  ];
  testHaskellDepends = [
    array base containers HUnit haskell-src-exts QuickCheck
    transformers
  ];
  description = "Tools for interacting with Bluesky / AT Protocol";
  license = lib.licenses.bsd3;
}
