{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-linux" ] (system:
    let
      packageName = "hcli";
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          ${packageName} =
            final.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              modules = [{
                # reinstall only cabal, exclude it from ghc reinstall
                nonReinstallablePkgs =
                  [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
                    "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
                    "ghcjs-prim" "ghcjs-th" "ghc-boot"
                    "ghc" "Win32" "array" "binary" "bytestring" "containers"
                    "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
                    "hpc" "mtl" "parsec" "process" "text" "time" "transformers"
                    "unix" "xhtml" "terminfo"
                  ];
                }];
              index-state = "2022-03-27T00:00:00Z";
              plan-sha256 = "1kl5rmvr19j4b3lw34s3i86g6wgdwd9k38yq1krbj5dqsr7hyy5j";
              shell = {
                tools = {
                  cabal = "3.6.2.0";
                  hlint = "latest";
                  haskell-language-server = "latest";
                  ormolu = "0.1.4.1";
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
