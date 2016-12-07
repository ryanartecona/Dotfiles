{
  allowUnfree = true;

  packageOverrides = (pkgs: rec {

    ra-volume-utils = pkgs.callPackage ./pkgs/ra-volume-utils.nix {};

  });
}
