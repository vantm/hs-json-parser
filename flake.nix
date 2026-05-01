{
  description = "JSON Parser";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
    in
    {
      devShells.${system}.default =
        let
          hPkgs = pkgs.haskell.packages."ghc9103";

          devTools = [
            hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
            hPkgs.ghcid # Continuous terminal Haskell compile checker
            hPkgs.fourmolu # Haskell formatter
            # hPkgs.hlint # Haskell codestyle checker
            hPkgs.hoogle # Lookup Haskell documentation
            hPkgs.haskell-language-server # LSP server for editor
            # hPkgs.cabal-install
            stack-wrapped
            pkgs.zlib # External C library needed by some Haskell packages
          ];

          # Wrap Stack to work with our Nix integration. We do not want to modify
          # stack.yaml so non-Nix users do not notice anything.
          # - no-nix: We do not want Stack's way of integrating Nix.
          # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
          # --no-install-ghc  # Do not try to install GHC if no matching GHC found on PATH
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack"; # will be available as the usual `stack` in terminal
            paths = [ hPkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --no-nix \
                  --system-ghc \
                  --no-install-ghc \
                "
            '';
          };
        in
        pkgs.mkShell {
          name = "JSON parser";
          buildInputs = devTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
        };
    };
}
