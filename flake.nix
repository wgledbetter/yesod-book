{
  description = "my project description";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";

  # When building myApp through pkgs.haskell.lib.buildStackProject (down below)
  # Stack should download Haskell packages directly from Stackage and bypass Nix,
  # so force Nix to allow this "non-sandboxed" build. See https://zimbatm.com/notes/nix-packaging-the-heretic-way
  nixConfig.sandbox = "relaxed";

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hPkgs = pkgs.haskell.packages."ghc965";

        myLibDeps = [
          pkgs.zlib # External C compression library needed by some Haskell packages
        ];

        myLocalDevTools = [
          hPkgs.ghc # GHC compiler in the version above; verify with `ghc --version`
          stack-wrapped
          #hPkgs.ghcid  # Continous terminal Haskell compile checker
          #hPkgs.ormolu # Haskell formatter
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          hPkgs.retrie # Haskell refactoring tool
          # hPkgs.cabal-install
        ];

        # Wrap Stack to work with our custom Nix integration. We don't modify stack.yaml so it keeps working for non-Nix users.
        # --no-nix         # Don't use Stack's built-in Nix integrating.
        # --system-ghc     # Use the existing GHC on PATH (will be provided through this Nix file)
        # --no-install-ghc # Don't try to install GHC if no matching GHC version found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
                --system-ghc \
              "
          '';
        };

        myApp = pkgs.haskell.lib.buildStackProject {
          name = "myStack";
          src = ./.;
          ghc = hPkgs.ghc;
          buildInputs = myLibDeps;
        };

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = myLocalDevTools;

          # Make external Nix C libraries like zlib known to GHC, like pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myLibDeps;
        };
        packages.default = myApp;
      });
}
