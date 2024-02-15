{ nix-helpers ? import ./nix-helpers.nix { }, lib ? nix-helpers.nixpkgs-lib }:
nix-helpers.callCabal2nixWithPlan {
  name = "json-to-msgpack";
  # Avoid having to rebuild after changing README, shell.nix, etc.
  src = lib.cleanSourceWith {
    filter = name: _:
      builtins.any (suf: lib.hasSuffix suf name) [
        ".hs"
        ".cabal"
        "json-to-msgpack"
        "src"
        "test"
      ];
    src = lib.cleanSource ./..;
  };
}
