{
  description = "Library Director - A Haskell Snap web server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            library-director = hself.callCabal2nix "library-director" ./backend { };
          };
        };

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell toolchain
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
            
            # Development tools
            zlib
            pkg-config
            entr
          ];

          shellHook = ''
            echo "🚀 Library Director development environment"
            echo ""
            echo "Commands:"
            echo "  cd backend && cabal run                                    - Start server"
            echo "  cd backend && find src src-main -name '*.hs' | entr -r cabal run  - Live reload"
          '';
        };

      in {
        packages.default = haskellPackages.library-director;
        
        devShells.default = devShell;

        apps.default = {
          type = "app";
          program = "${haskellPackages.library-director}/bin/library-director";
        };
      }
    );
}

