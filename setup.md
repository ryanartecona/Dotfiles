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
rm -rf "~/Library/Application Support/Code/User/snippets"
ln -sf ~/Dotfiles/VSCode/settings.json "~/Library/Application Support/Code/User/settings.json"
ln -sf ~/Dotfiles/VSCode/keybindings.json "~/Library/Application Support/Code/User/keybindings.json"
ln -sf ~/Dotfiles/VSCode/snippets "~/Library/Application Support/Code/User/snippets"
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
for fn in (ls functions)
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

## Karabiner

```
# symlink the whole config dir, or else karabiner won't play nice.
rm -rf ~/.config/karabiner
ln -sf ~/Dotfiles/karabiner ~/.config/karabiner
```
