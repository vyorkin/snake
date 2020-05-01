let lib = (import ./release.nix).haskell.lib;
in (super: {
  sdl2 = lib.dontCheck (super.callPackage ./deps/sdl2.nix {});
  linear = lib.dontCheck (super.callPackage ./deps/linear.nix {});
  apecs-random = lib.dontCheck (super.callCabal2nix "apecs-random" ../vendor/apecs-random { });
})
