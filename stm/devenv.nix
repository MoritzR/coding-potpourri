{ pkgs, ... }:
{
  packages = with pkgs; [
    haskellPackages.fourmolu
  ];
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc912;
  };
}
