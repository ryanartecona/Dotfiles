# https://github.com/NixOS/nixpkgs/issues/24717#issuecomment-294109450
self: super:
{
  # add man to the outputs to install for nix
  nixWithManual = super.nix.overrideAttrs (oldAttrs: {
     meta = oldAttrs.meta // {
       outputsToInstall = [ "out" "man" ];
     };
  });
}
