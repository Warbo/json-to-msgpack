with import <nixpkgs> {};
runCommand "m"
  {
    buildInputs = [
      (haskellPackages.ghcWithPackages (h: [
        h.bits h.bytestring h.data-msgpack h.data-msgpack-types h.tasty
        h.tasty-quickcheck
        (haskell.lib.dontCheck haskellPackages.tasty-discover)
      ]))
    ];
  }
  "exit 1"
