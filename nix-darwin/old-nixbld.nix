{ ... }:
{
  # default nix installer uses gid 350 for nixbld group, older installations used 30000
  ids.gids.nixbld = 30000;
}
