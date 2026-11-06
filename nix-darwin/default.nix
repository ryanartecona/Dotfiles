# A top level nix-darwin config for each computer with different needs.
# Structure inspired by https://github.com/MatthiasBenaets/nix-config/blob/master/darwin/default.nix
# Use like `sudo darwin-rebuild switch --flake .#home-studio`.
{
  nixpkgs,
  nixpkgs-stable,
  nix-darwin,
  home-manager,
  pi,
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
      piPackage = pi.packages.${system}.default;
      # default
      primaryUser = "ryanartecona";
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
        ./homebrew.nix
        home-manager.darwinModules.home-manager
      ];
    };

  work-2022 =
    let
      config = systemConfig "aarch64-darwin";
    in
    with config;
    nix-darwin.lib.darwinSystem {
      inherit system;
      specialArgs = config;
      modules = [
        ./common.nix
        ./homebrew.nix
        ./old-nixbld.nix
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
      specialArgs = config // {
        primaryUser = "ra";
      };
      modules = [
        ./common.nix
        home-manager.darwinModules.home-manager
        ({ lib, pkgs, ... }: {
          # use mise, which conflicts with direnv
          environment.systemPackages = with pkgs; [
            # TEMP: pull newer mise from upstream master until nixpkgs-unstable
            # catches up past 2026.10.17
            (mise.overrideAttrs (old: rec {
              version = "2026.11.3";
              src = pkgs.fetchFromGitHub {
                owner = "jdx";
                repo = "mise";
                tag = "v${version}";
                hash = "sha256-rH9yZQIxpTqG0qSJhX0BtZ61qoQPf0vo/hCMmFXTZPM=";
              };
              cargoHash = "sha256-lrzYiAQwBru6BilEgaNBGUyb8giVAoFqmHDboPSHhvs=";
            }))
          ];
          programs.direnv.enable = lib.mkForce false;
        })
      ];
    };
}
