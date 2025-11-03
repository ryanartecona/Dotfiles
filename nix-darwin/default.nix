# A top level nix-darwin config for each computer with different needs.
# Structure inspired by https://github.com/MatthiasBenaets/nix-config/blob/master/darwin/default.nix
# Use like `sudo darwin-rebuild switch --flake .#home-studio`.
{
  nixpkgs,
  nixpkgs-stable,
  nix-darwin,
  home-manager,
  ...
}:

let
  systemConfig =
    system:
    let
      nixpkgsArgs = {
        system = system;
        config.allowUnfree = true;
        overlays = [
          (import ../nixpkgs/overlays/50-ra-scripts.nix)
        ];
      };
    in
    {
      system = system;
      pkgs = import nixpkgs nixpkgsArgs;
      stable = import nixpkgs-stable nixpkgsArgs;
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
        home-manager.darwinModules.home-manager
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
        home-manager.darwinModules.home-manager
      ];
    };
}
