{
  description = "RA's macOS config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.05";

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      # make nix-darwin's nixpkgs use this flake's above
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Pi coding agent, tracking the stable branch. Upstream's nix.yml workflow
    # builds every v* tag on aarch64-darwin and only fast-forwards stable to a
    # tag after the build + smoke test pass, so stable never points at a
    # nix-broken release. flake.lock still pins the exact revision, so pi only
    # moves on `nix flake update pi`. To pin an exact release instead, use a
    # tag url (e.g. github:earendil-works/pi/v1.3.0).
    pi.url = "github:earendil-works/pi/stable";
  };

  outputs = inputs @ { self, nixpkgs, nixpkgs-stable, ... }: {
    darwinConfigurations = import ./nix-darwin inputs;
  };
}
