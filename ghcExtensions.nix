{pkgs, ...}:
hfinal: hprev:
with pkgs.haskell.lib.compose;
{
  smol = hfinal.callCabal2nix "smol" (pkgs.lib.cleanSource ./.) {};
}
