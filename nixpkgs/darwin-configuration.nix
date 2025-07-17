{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [ 
    vim
    direnv
    nixpkgs-fmt
  ];

  nixpkgs.config.allowUnfree = true;

  system.primaryUser = "ryanartecona";

  services.lorri.enable = true;

  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];

  programs._1password.enable = true;
  programs.fish.enable = true;
  programs.direnv.enable = true;

  environment.shells = [
    pkgs.fish
  ];

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # default nix installer uses gid 350 for nixbld group
  ids.gids.nixbld = 350;
}
