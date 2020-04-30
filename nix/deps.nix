let lib = (import ./release.nix).haskell.lib;
in (super: {
  apecs-random = lib.dontCheck
  (super.callCabal2nix "apecs-random" ../vendor/apecs-random { });
})
