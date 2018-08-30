{
  hsPkgs ? (pkgs.haskellPackages.override (_: {
             overrides = self: super: {
               json-to-msgpack = self.callPackage
                 (self.haskellSrc2nix {
                   name = "json-to-msgpack";
                   src  = ./.;
                 })
                 {};

               hedgehog = pkgs.haskell.lib.dontCheck super.hedgehog;
             };
           })),

  json-to-msgpack ? hsPkgs.json-to-msgpack,
  lzip            ? pkgs.lzip,
  pkgs            ? import <nixpkgs> {},
  runCommand      ? pkgs.runCommand
}:

runCommand "test-json-to-msgpack"
  {
    buildInputs = [ json-to-msgpack lzip ];
    f = ./test/example.json.lz;
  }
  ''
    lzip -d < "$f" > example.json
    json-to-msgpack example.json | lzip > "$out"
  ''
