{ config, pkgs, ... }:

# Reference: https://nix-darwin.github.io/nix-darwin/manual/
{
  nix = {
    package = pkgs.nix;
    gc = {
      automatic = true;
      interval.Day = 7;
      options = "--delete-older-than 7d";
    };
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Use the pkgs from the nix-darwin flake for all darwin modules, i.e. home-manager
  nixpkgs.pkgs = pkgs;

  imports = [
    ./aerospace.nix
  ];

  # $ nix-env -qaP | grep wget
  # List packages installed in system profile. To search by name, run:
  environment.systemPackages = with pkgs; [
    vim
    direnv
    nixfmt-rfc-style
    nix-output-monitor
    exiftool
    (pkgs.callPackage ../nix/exif.nix { })
    # xz is a hidden dependency of nvm when gnutar also installed on macos
    # ([thread](https://github.com/nvm-sh/nvm/issues/3034#issuecomment-1694564861))
    xz
  ];

  system.primaryUser = "ryanartecona";

  services.lorri.enable = true;

  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];

  homebrew.enable = true;
  homebrew.brews = [
    "nvm"
  ];
  homebrew.casks = [
    "iterm2"
    "karabiner-elements"
    "alfred@4"
  ];

  programs._1password.enable = true;
  programs.fish.enable = true;
  programs.direnv.enable = true;

  environment.shells = [
    pkgs.fish
    # this is where nix-env installs fish
    "/Users/ryanartecona/.nix-profile/bin/fish"
    # this is where home-manager installs fish
    "/etc/profiles/per-user/ryanartecona/bin/fish"
  ];

  users.users.ryanartecona = {
    name = "ryanartecona";
    home = "/Users/ryanartecona";
  };

  # home-manager configuration
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.ryanartecona = import ../home-manager/home.nix;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
}
