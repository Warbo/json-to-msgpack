{ json-to-msgpack ? import nix/json-to-msgpack.nix { inherit nix-helpers; }
, nix-helpers ? import nix/nix-helpers.nix { }
, cabal-install ? nix-helpers.nixpkgs.cabal-install
, lzip ? nix-helpers.nixpkgs.lzip }:
json-to-msgpack.haskellPackages.shellFor {
  packages = p: [ p.json-to-msgpack ];
  withHoogle = true;
  buildInputs = [ cabal-install lzip ];
}
