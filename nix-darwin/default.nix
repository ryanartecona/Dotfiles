# A top level nix-darwin config for each computer with different needs.
# Structure inspired by https://github.com/MatthiasBenaets/nix-config/blob/master/darwin/default.nix
# Use like `sudo darwin-rebuild switch --flake .#home-studio`.
{ nixpkgs, nixpkgs-stable, nix-darwin, ... }:

let
  systemConfig = system: {
    system = system;
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
    stable = import nixpkgs-stable {
      inherit system;
      config.allowUnfree = true;
    };
  };

in
{
  home-studio = 
    let
      config = systemConfig "aarch64-darwin";
    in
    with config;
      nix-darwin.lib.darwinSystem {
        inherit system;
        specialArgs = config;
        modules = [
          ./common.nix
        ];
      };

  work = 
    let
      config = systemConfig "aarch64-darwin";
    in
      with config;
      nix-darwin.lib.darwinSystem {
        inherit system;
        specialArgs = config;
        modules = [
          ./common.nix
          ./old-nixbld.nix
        ];
      };
}
