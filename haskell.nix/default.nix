let
  # Fetch the latest haskell.nix and import its default.nix
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
  # you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'
  nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
  # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
  # and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ pkgs ? import nixpkgsSrc nixpkgsArgs
, haskellCompiler ? "ghc883"
}:
# 'cabalProject' generates a package set based on a cabal.project (and the corresponding .cabal files)
pkgs.haskell-nix.cabalProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};

  # Additional packages ontop of all those listed in `cabal.project`
  pkg-def-extras = [
    (_: pkgs.haskell-nix.cabalProject { src = ./vendor/apecs-random; })
  ];

  # modules = [
  #   # Specific package overrides would go here for example:
  #   packages.cbors.package.ghcOptions = "-Werror";
  #   packages.cbors.patches = [ ./one.patch ];
  #   packages.cbors.flags.optimize-gmp = false;
  #   # It may be better to set flags in `cabal.project` instead
  #   # (`plan-to-nix` will include them as defaults).
  # ];
}
