let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz";
  }) {};

  # 2020-03-16 nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "47507b27e15a9c3b929111cf43d2e9c0e4b97f4c";
    sha256 = "0gnwymgm4i5y9vknpcsr99pwy76w14nclqxb6xmmzlw2s8fx85hm";
  }) {};

in pkgs.stdenv.mkDerivation {
  name = "mastodon-emoji-viewer";
  buildInputs = with pursPkgs; [
    purs spago purty pscid
    pkgs.nodejs-12_x
  ];
}