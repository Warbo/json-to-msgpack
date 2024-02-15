{ json-to-msgpack ? import ./. { }, nix-helpers ? import nix/nix-helpers.nix { }
, nixpkgs ? nix-helpers.nixpkgs, lzip ? nixpkgs.lzip
, runCommand ? nixpkgs.runCommand }:

runCommand "test-json-to-msgpack" {
  buildInputs = [ json-to-msgpack lzip ];
  f = ./test/example.json.lz;
} ''
  lzip -d < "$f" > example.json
  json-to-msgpack example.json | lzip > "$out"
''
