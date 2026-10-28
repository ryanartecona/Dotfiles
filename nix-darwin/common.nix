{ config, pkgs, primaryUser ? "ryanartecona", piPackage ? null, ... }:

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
    # necessary to build x86 on Apple Silicon macs
    settings.extra-platforms = [ "x86_64-darwin" "aarch64-darwin" ];
  };

  # Use the pkgs from the nix-darwin flake for all darwin modules, i.e. home-manager
  nixpkgs.pkgs = pkgs;

  imports = [
    ./osx-defaults.nix
    ./aerospace.nix
  ];

  # $ nix-env -qaP | grep wget
  # List packages installed in system profile. To search by name, run:
  environment.systemPackages = with pkgs; [
    vim
    direnv
    nixfmt
    nix-output-monitor
    exiftool
    # GUI app: installed here so it lands in /Applications/Nix Apps. Its config
    # lives in home-manager/home.nix.
    # Pinned ahead of nixpkgs to 1.4.0 (released 2026-10-27); nixpkgs unstable
    # was still on 1.3.1. Remove this override once nixpkgs catches up.
    # TODO: remove override when pkgs.ghostty-bin >= 1.4.0
    (ghostty-bin.overrideAttrs (old: rec {
      version = "1.4.0";
      src = pkgs.fetchurl {
        url = "https://release.files.ghostty.org/${version}/Ghostty.dmg";
        hash = "sha256-OgIHXb0kMY13hCavQd7lUH1T7MRXjt6WkS7Jzm0Y0+g=";
      };
    }))
    (pkgs.callPackage ../nix/exif.nix { })
    # xz is a hidden dependency of nvm when gnutar also installed on macos
    # ([thread](https://github.com/nvm-sh/nvm/issues/3034#issuecomment-1694564861))
    xz
  ];

  system.primaryUser = primaryUser;

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
    # this is where nix-env installs fish
    "/Users/${primaryUser}/.nix-profile/bin/fish"
    # this is where home-manager installs fish
    "/etc/profiles/per-user/${primaryUser}/bin/fish"
  ];

  users.users.${primaryUser} = {
    name = primaryUser;
    home = "/Users/${primaryUser}";
  };

  # home-manager configuration
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.${primaryUser} = import ../home-manager/home.nix { inherit primaryUser piPackage; };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;
}
