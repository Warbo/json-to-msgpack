{ json-to-msgpack ? import ./json-to-msgpack.nix { }
, nix-helpers ? import ./nix-helpers.nix { }, nixpkgs ? nix-helpers.nixpkgs }:
nixpkgs.runCommand "test-json-to-msgpack" {
  buildInputs = [ json-to-msgpack nixpkgs.lzip ];
  f = ./example.json.lz;
} ''
  lzip -d < "$f" > example.json
  json-to-msgpack example.json | lzip > "$out"
''
