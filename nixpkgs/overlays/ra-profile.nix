self: super:
{
  ra-profile = super.buildEnv {
    name = "ra-profile";

    # e.g. moreutils' `parallel` and GNU `parallel` would collide.
    ignoreCollisions = true;

    paths = with super; [
      awscli
      bind
      cmake
      coreutils
      elixir
      entr
      figlet
      findutils
      fish
      fzf
      gitFull
      gnused
      gnutar
      go
      graphviz
      htop
      httpie
      gitAndTools.hub
      jq
      moreutils
      nix-prefetch-scripts
      nodejs-slim-9_x
      nsq
      ocaml
      opam
      pandoc
      parallel
      pinentry_mac
      ranger
      ripgrep
      sloccount
      tig
      wget
      yarn

      # From my overlays
      ra-e
    ];
  };
}