{
  description = "Generate tags file for Haskell project and its nearest deps";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
      pkgsFor = system: import nixpkgs { inherit system; };
      drvFor = system: import ./default.nix { nixpkgs = pkgsFor system; };
    in
    {
      packages = forAllSystems (system: {
        default = drvFor system;
      });
      devShells = forAllSystems (system: {
        default = (drvFor system).env;
      });
      formatter = forAllSystems (system: (pkgsFor system).nixfmt);
    };
}
