{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "ryanartecona";
  home.homeDirectory = "/Users/ryanartecona";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "25.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # enable fish integration
  programs.fish.enable = true;
  programs.fish.shellInit = builtins.readFile ../fish/config.fish;

  # dirs to put on $PATH. note home-manager prepends these, but then my 
  # config.fish sorts them back to the bottom.
  home.sessionPath = [
    "$HOME/bin"
    "${config.home.profileDirectory}/bin"
  ];

  # Packages to install in the user profile.
  # This replaces the ra-profile overlay.
  home.packages = with pkgs; [
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
    gh
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
    # nodejs-slim-18_x
    # nsq
    neovim
    ocaml
    opam
    pandoc
    parallel
    pinentry_mac
    python3
    # pythonPackages.pygments
    ranger
    ripgrep
    terraform
    tig
    tree
    wget
    # yarn

    # From my ra-scripts overlay
    ra-e
    ra-echo-argv
  ];
}
