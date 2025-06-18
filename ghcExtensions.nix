{pkgs, ...}:
hfinal: hprev:
with pkgs.haskell.lib.compose;
{
  mkDerivation = args: hprev.mkDerivation (args // {
    # Since we are forcing our ideas upon mkDerivation, this change will
    # affect every package in the package set.
    enableLibraryProfiling = true;

    # To actually use profiling on an executable, executable profiling
    # needs to be enabled for the executable you want to profile. You
    # can either do this globally or…
    enableExecutableProfiling = false;
  });

  smol = hfinal.callCabal2nix "smol" (pkgs.lib.cleanSource ./.) {};
}
