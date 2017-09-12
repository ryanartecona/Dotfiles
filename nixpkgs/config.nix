{
  allowUnfree = true;

  packageOverrides = (pkgs: rec {

    ra-volume-utils = pkgs.callPackage ./pkgs/ra-volume-utils.nix {};

    # ra-zoom-us = pkgs.libsForQt56.callPackage ./pkgs/zoom-us.nix {};

  });
}
