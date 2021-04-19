let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
  }) {};

  # 2021-04-19 nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "734ca9c00038f4b549bf8fc58eb65e08a87e9d56";
    sha256 = "1cn2gmd55bcx97xi7v59m4xw4y86v60p85x460jbb8bn6cfy6xmc";
  }) {};

in pkgs.stdenv.mkDerivation {
  name = "mastomojo";
  buildInputs = with pursPkgs; [
    pursPkgs.purs
    pursPkgs.spago
    pursPkgs.zephyr
    pkgs.nodejs-14_x
  ];
}
