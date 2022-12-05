{
  nixConfig = {
    extra-substituters = "https://cache.iog.io";
    extra-trusted-public-keys = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=";
  };  

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      packageName = "hcli";
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          ${packageName} =
            final.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = "ghc925";
              shell = {
                tools = {
                  cabal = "3.8.1.0";
                  hlint = "latest";
                  haskell-language-server = "latest";
                  ormolu = "0.5.0.1";
                };
                buildInputs = [pkgs.haskellPackages.implicit-hie];
                withHoogle = true;
              };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.${packageName}.flake {};
    in flake // {      
      # Built by `nix build .`
      defaultPackage = flake.packages."${packageName}:exe:${packageName}";
      # nix build .#calculateMaterializedSha.x86_64-linux --json | jq '.[]|outputs.out' | bash
      calculateMaterializedSha = pkgs.${packageName}.plan-nix.passthru.calculateMaterializedSha;
    });
}
