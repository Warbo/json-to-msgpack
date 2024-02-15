{ nix-helpers ? import nix/nix-helpers.nix { }
, callCabal2nixWithPlan ? nix-helpers.callCabal2nixWithPlan
, lib ? nix-helpers.nixpkgs-lib }:
callCabal2nixWithPlan {
  name = "json-to-msgpack";
  src = lib.cleanSource ./.;
}
