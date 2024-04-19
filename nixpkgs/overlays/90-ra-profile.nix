self: super:
{
  ra-profile = super.buildEnv {
    name = "ra-profile";

    # e.g. moreutils' `parallel` and GNU `parallel` would collide.
    # ignoreCollisions = true;

    extraOutputsToInstall = [ "man" ];

    paths = with super; [
      asciinema
      asciidoc
      awscli
      bat
      # bind # depends on broken llvm package 2022-09-18
      cmake
      coreutils
      ctags
      direnv
      dune_2
      # elixir
      entr
      fd
      figlet
      findutils
      fish
      fswatch
      fzf
      gitFull
      git-crypt
      # gnused
      gnutar
      go
      graphviz
      htop
      httpie
      gitAndTools.hub
      jq
      lorri
      # moreutils # conflicts with GNU parallel below
      # nasm
      nix-prefetch-scripts
      nixfmt
      # nodejs-slim-18_x
      # nsq
      ocaml
      opam
      pandoc
      parallel
      pinentry_mac
      python3
      # pythonPackages.pygments
      ranger
      ripgrep
      sloccount
      terraform
      tig
      tree
      wget
      # yarn

      # From my ra-scripts overlay
      ra-e
      ra-echo-argv
    ];
  };
}
