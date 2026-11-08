{
  primaryUser ? "ryanartecona",
  dotfilesPath ? "/Users/${primaryUser}/Dotfiles",
  piPackage ? null,
}:
{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = primaryUser;
  home.homeDirectory = "/Users/${primaryUser}";

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
    # awscli
    bat
    # bind # depends on broken llvm package 2022-09-18
    cmake
    coreutils
    ctags
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
    hub
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
    patch
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
  ]
  # Pi coding agent, from the flake input pinned in flake.nix (null when the
  # input isn't wired through, e.g. standalone home-manager use).
  ++ lib.optional (piPackage != null) piPackage;

  # Ghostty terminal config. The app itself (ghostty-bin) is installed by
  # nix-darwin (environment.systemPackages) so it lands in /Applications/Nix
  # Apps. Config is a live symlink below rather than programs.ghostty.settings,
  # so it can be edited without a rebuild.
  programs.ghostty = {
    enable = true;
    package = null;
  };

  xdg.configFile."ghostty/config".source =
    config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/ghostty/config";

  # Pi (pi.dev) config: symlink live files from this repo instead of copying
  # into the nix store, so edits take effect immediately without a rebuild.
  # Secrets (auth.json, mcp-auth.json) and pi-managed state (install/, npm/,
  # sessions/) are deliberately not managed here.
  #
  # ~/.pi/agent/extensions is a real directory with one live symlink per entry
  # (not a symlink to the whole repo dir) so tools like Corgi can drop their own
  # extension in it: `corgi integration install pi` refuses a symlinked
  # extensions dir (SharedExtensionsDirectory), which left Pi status in Corgi's
  # sidebar stuck on "Starting session". Entries are enumerated from the flake
  # source, so a *new* extension needs `git add` + a rebuild; edits to existing
  # ones (and anything inside an extension directory) stay live. ~/.pi/agent/skills
  # and ~/.pi/agent/agents are deployed the same way (no third-party writer, but
  # enumeration keeps one pattern).
  home.file =
    let
      live = rel: config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/${rel}";
      piExtensions = lib.mapAttrs' (
        name: _:
        lib.nameValuePair ".pi/agent/extensions/${name}" { source = live "pi/agent/extensions/${name}"; }
      ) (builtins.readDir ../pi/agent/extensions);
      piSkills = lib.mapAttrs' (
        name: _:
        lib.nameValuePair ".pi/agent/skills/${name}" { source = live "pi/agent/skills/${name}"; }
      ) (builtins.readDir ../pi/agent/skills);
      piAgents = lib.mapAttrs' (
        name: _:
        lib.nameValuePair ".pi/agent/agents/${name}" { source = live "pi/agent/agents/${name}"; }
      ) (builtins.readDir ../pi/agent/agents);
    in
    piExtensions
    // piSkills
    // piAgents
    // {
      "bin/roadmap-lint".source = live "pi/bin/roadmap-lint";
      ".pi/agent/settings.json".source = live "pi/agent/settings.json";
      ".pi/agent/mcp.json".source = live "pi/agent/mcp.json";
      ".pi/agent/keybindings.json".source = live "pi/agent/keybindings.json";
      ".pi/agent/compact-tools.json".source = live "pi/agent/compact-tools.json";
      ".pi/agent/pledit.json".source = live "pi/agent/pledit.json";
      ".pi/agent/open-tui.json".source = live "pi/agent/open-tui.json";
      ".pi/agent/subagents.json".source = live "pi/agent/subagents.json";
      ".pi/agent/tmux-mcp.toml".source = live "pi/agent/tmux-mcp.toml";
      ".pi/agent/zentui.json".source = live "pi/agent/zentui.json";
      ".pi/agent/npm/package.json".source = live "pi/agent/npm/package.json";
      ".pi/agent/npm/package-lock.json".source = live "pi/agent/npm/package-lock.json";
      ".pi/mcp-adapter.json".source = live "pi/mcp-adapter.json";
    };

  # One-time migration: the old whole-directory symlink would make the new
  # per-file links collide with the repo files it points at. Remove it (only if
  # it is a symlink) before Home Manager checks link targets.
  home.activation.piExtensionsRealDir = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
    if [ -L "$HOME/.pi/agent/extensions" ]; then
      run rm "$HOME/.pi/agent/extensions"
    fi
  '';
}
