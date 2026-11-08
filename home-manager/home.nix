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

let
  # Live (out-of-store) symlink to a path in this repo, so edits take effect
  # immediately without a rebuild.
  live = rel: config.lib.file.mkOutOfStoreSymlink "${dotfilesPath}/${rel}";

  # home.file entries for a list of $HOME-relative paths whose repo path is
  # the same with the leading dot stripped: ".tigrc" -> "tigrc",
  # ".pi/agent/settings.json" -> "pi/agent/settings.json".
  dotfilesRelativeSymlinks = homePaths: lib.listToAttrs (map
    (homePath: lib.nameValuePair homePath { source = live (lib.removePrefix "." homePath); })
    homePaths);
in

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
    # tree-sitter CLI: only needed for :TSInstallFromGrammar, but silences the
    # last nvim-treesitter :checkhealth warning
    tree-sitter
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

  xdg.configFile."ghostty/config".source = live "ghostty/config";

  # Neovim config: live-symlink the whole nvim/ dir from this repo (replaces
  # the manual `ln -s ~/Dotfiles/nvim ~/.config/nvim` step from setup.md).
  # neovim itself is in home.packages above.
  xdg.configFile."nvim".source = live "nvim";

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
    // dotfilesRelativeSymlinks [
      ".pi/agent/settings.json"
      ".pi/agent/mcp.json"
      ".pi/agent/keybindings.json"
      ".pi/agent/compact-tools.json"
      ".pi/agent/pledit.json"
      ".pi/agent/open-tui.json"
      ".pi/agent/subagents.json"
      ".pi/agent/tmux-mcp.toml"
      ".pi/agent/zentui.json"
      ".pi/agent/npm/package.json"
      ".pi/agent/npm/package-lock.json"
      ".pi/mcp-adapter.json"
      ".tigrc"
    ]
    // {
      # repo path isn't just the home path minus the leading dot
      "bin/roadmap-lint".source = live "pi/bin/roadmap-lint";
      ".pi-lens/config.json".source = live "pi/pi-lens/config.json";
    };

  # One-time migration: the old whole-directory symlink would make the new
  # per-file links collide with the repo files it points at. Remove it (only if
  # it is a symlink) before Home Manager checks link targets.
  home.activation.piExtensionsRealDir = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
    if [ -L "$HOME/.pi/agent/extensions" ]; then
      run rm "$HOME/.pi/agent/extensions"
    fi
  '';

  # One-time migration: ~/.config/nvim used to be a manual symlink straight to
  # ~/Dotfiles/nvim (per old setup.md). Remove it (only if it is a symlink) so
  # Home Manager can install its own out-of-store link instead.
  home.activation.nvimConfigSymlink = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
    if [ -L "$HOME/.config/nvim" ]; then
      run rm "$HOME/.config/nvim"
    fi
  '';

  # ~/.pi-lens/config.json predates Home Manager management as a real file;
  # move it aside (once) so the live symlink above can take its place. The rest
  # of ~/.pi-lens is pi-lens-managed state and stays untouched.
  home.activation.piLensConfigRealFile = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
    if [ -f "$HOME/.pi-lens/config.json" ] && [ ! -L "$HOME/.pi-lens/config.json" ]; then
      run mv "$HOME/.pi-lens/config.json" "$HOME/.pi-lens/config.json.bak"
    fi
  '';
}
