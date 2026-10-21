{ ... }:

{
  homebrew.enable = true;
  homebrew.brews = [
    "nvm"
    "awscli"
  ];
  homebrew.casks = [
    "iterm2"
    "karabiner-elements"
    "alfred"
  ];
}
