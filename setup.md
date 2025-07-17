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
- Ayu theme (Mirage)
- direnv
- edamagit
- File Browser
- Fish
- GitLens
- Nix IDE
- Vim

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

Install with the following (via https://github.com/LnL7/nix-darwin):
```
nix-build '<darwin>' -A darwin-rebuild
sudo ./result/bin/darwin-rebuild switch -I darwin-config=/etc/nix-darwin/configuration.nix
```

Then, after making a change to darwin-configuration.nix, apply it with this:
```
darwin-rebuild switch
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
