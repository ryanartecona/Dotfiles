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

    # Pi coding agent, pinned to a main-branch rev. Rationale:
    # - the v1.2.0 tag's nix build is broken (installCheckPhase requires
    #   esbuild, which 1.2.0 dropped from its install-lock)
    # - main HEAD b06c1f2 (2026-10-14) is also broken (kimi-coding provider
    #   switched to openai-completions without regenerating model data)
    # - this rev is main~1: has the esbuild fix, predates the kimi breakage
    # To upgrade: `nix flake update pi` after switching the url to a branch
    # (e.g. .../pi/main or .../pi/stable), or swap in a newer rev/tag.
    pi.url = "github:earendil-works/pi/87f9a36419e8164bba77397d15c54ff39fac5e8a";
  };

  outputs = inputs @ { self, nixpkgs, nixpkgs-stable, ... }: {
    darwinConfigurations = import ./nix-darwin inputs;
  };
}
