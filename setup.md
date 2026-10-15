# setting up a new computer

Collecting notes, because it's annoying to relearn this stuff but I don't do it enough to automate it.

## Nix

Install with `curl -L https://nixos.org/nix/install | sh`.

```
# link the whole config dir
ln -sf ~/Dotfiles/nixpkgs ~/.config/nixpkgs

# subscribe to the unstable channel
nix-channel --add https://nixos.org/channels/nixpkgs-unstable

# install my shit
nix-env -iA nixpkgs.ra-profile
```

## VS Code

```sh
# link the important config files
rm -rf "$HOME/Library/Application Support/Code/User/snippets"
ln -sf ~/Dotfiles/VSCode/settings.json "$HOME/Library/Application Support/Code/User/settings.json"
ln -sf ~/Dotfiles/VSCode/keybindings.json "$HOME/Library/Application Support/Code/User/keybindings.json"
ln -sf ~/Dotfiles/VSCode/snippets "$HOME/Library/Application Support/Code/User/snippets"
```

Install extenions:
- advanced-new-file
- Gruvbox theme
- direnv
- edamagit
- File Browser
- Fish
- GitLens
- Nix IDE
- NeoVim

## Fish shell

Fish executable installed via Nix profile.

```fish
# symlink config files
ln -sf ~/Dotfiles/fish/config.fish ~/.config/fish/config.fish
for fn in (ls ~/Dotfiles/fish/functions)
  ln -sf ~/Dotfiles/fish/functions/$fn ~/.config/fish/functions/$fn
end

# install plugin manager "fisher"
curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
ln -sf ~/Dotfiles/fish/fish_plugins ~/.config/fish/fish_plugins
fisher update
```

Functions are symlinked individually, so new ones must be synced when added.

## Git

```fish
ln -s ~/Dotfiles/gitconfig ~/.gitconfig
mkdir -p ~/.config/git
ln -s ~/Dotfiles/gitignore ~/.config/git/ignore
```

## Hammerspoon

Mainly used for window movement hotkeys.

```
mkdir -p ~/.hammerspoon
ln -sf ~/Dotfiles/hammerspoon/init.lua ~/.hammerspoon/init.lua
```

## iTerm2

```sh
ln -s ~/Dotfiles/iTerm2/DynamicProfiles/mint_chocolate.json ~/Library/Application\ Support/iTerm2/DynamicProfiles/mint_chocolate.json
ln -s ~/Dotfiles/iTerm2/DynamicProfiles/mint_chocolate_scratchpad.json ~/Library/Application\ Support/iTerm2/DynamicProfiles/mint_chocolate_scratchpad.json
```

Then go into iTerm2 Preferences > Profiles > Mint Chocolate > Other Actions and set as default.

## Karabiner

```
# symlink the whole config dir, or else karabiner won't play nice.
rm -rf ~/.config/karabiner
ln -sf ~/Dotfiles/karabiner ~/.config/karabiner
```

## nix-darwin

Add nix-darwin channel
```
sudo nix-channel --add https://github.com/nix-darwin/nix-darwin/archive/master.tar.gz darwin
sudo nix-channel --update
```

Link config file:
```
sudo mkdir -p /etc/nix-darwin
sudo ln -sf ~/Dotfiles/nixpkgs/darwin-configuration.nix /etc/nix-darwin/configuration.nix
```

Install (first time) with the following (via https://github.com/LnL7/nix-darwin):
```
nix-build '<darwin>' -A darwin-rebuild
sudo ./result/bin/darwin-rebuild switch --flake .#home-studio
```

Then, after making a change to darwin-configuration.nix, apply it with this:
```
sudo darwin-rebuild switch --flake .#home-studio
```

nix-darwin can't change a user's shell, so also run this to use fish:
```
chsh -s /run/current-system/sw/bin/fish
```

## Direnv

```
mkdir -p ~/.config/direnv
ln -s ~/Dotfiles/direnvrc ~/.config/direnv/direnvrc
```

## Alfred

```
ln -sf ~/Dotfiles/alfred/Alfred.alfredpreferences ~/Library/Application\ Support/Alfred/
```

## Neovim

```
ln -s ~/Dotfiles/nvim ~/.config/nvim
```

## Pi

First, run pi's official installer (https://pi.dev) — it recreates `~/.pi/agent/install/`, `~/.pi/agent/bin/`, and its bundled node.

On nix machines, home-manager creates the config symlinks (see the `home.file` block in `home-manager/home.nix`) on the next `darwin-rebuild switch`. They are out-of-store symlinks pointing at the live files in this repo, so editing pi config takes effect immediately with no rebuild.

On non-nix machines, symlink manually:
```
ln -sf ~/Dotfiles/pi/mcp-adapter.json ~/.pi/mcp-adapter.json
for f in settings.json mcp.json keybindings.json compact-tools.json pledit.json open-tui.json zentui.json; do
  ln -sf ~/Dotfiles/pi/agent/$f ~/.pi/agent/$f
done
ln -sf ~/Dotfiles/pi/agent/extensions ~/.pi/agent/extensions
```

Launch `pi` once — it reads the `packages` list from `settings.json` and installs extension packages into `~/.pi/agent/npm/`.

Then re-authenticate: run pi's login flow (recreates `auth.json`), and redo MCP OAuth flows as needed (recreates `mcp-auth.json`). `auth.json` and `mcp-auth.json` contain secrets and are deliberately not in the repo.

Notes:
- `mcp.json` references `${SLACK_MCP_CLIENT_ID}` / `${SLACK_MCP_CLIENT_SECRET}` — make sure those are exported in the environment (e.g. from fish config or an untracked local file).
- `settings.json` lists `../../code/ra/pi-tmux-mcp/pi` in its `packages`, so clone that repo to the same relative path (`~/code/ra/pi-tmux-mcp/pi`) or edit the entry.
