self: super:
{
  ra-profile = super.buildEnv {
    name = "ra-profile";

    # e.g. moreutils' `parallel` and GNU `parallel` would collide.
    # ignoreCollisions = true;

    extraOutputsToInstall = [ "man" ];

    paths = with super; [
      asciinema
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
      # moreutils # conflicts with GNU parallel below
      nix-prefetch-scripts
      nodejs-slim-10_x
      nsq
      ocaml
      opam
      pandoc
      parallel
      pinentry_mac
      python3
      python3Packages.pylint
      ranger
      ripgrep
      sloccount
      terraform-full
      tig
      wget
      yarn

      # From my ra-scripts overlay
      ra-e
      ra-echo-argv
    ];
  };
}