{ nix-helpers ? import nix/nix-helpers.nix { }, nixpkgs ? nix-helpers.nixpkgs }:
with rec {
  callPackage = nixpkgs.newScope { inherit json-to-msgpack nix-helpers; };
  json-to-msgpack = callPackage nix/json-to-msgpack.nix { };
  test = callPackage nix/test.nix { };
};
json-to-msgpack.overrideAttrs
(old: { extraTests = (old.extraTests or [ ]) ++ [ test ]; })
