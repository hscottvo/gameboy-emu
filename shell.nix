{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    cargo
    cargo-watch
    lld
    rustc
    SDL
    sdl3
    wasm-pack
  ];
}
# { pkgs ? import <nixpkgs> { }, home-manager ? import <home-manager/nixpkgs> { } }:
#
# pkgs.mkShell
# {
#   buildInputs = with pkgs; [
#     cargo
#     cargo-watch
#     lld
#     rustc
#     wasm-pack
#   ] ++ (if pkgs.stdenv.isDarwin then [ ] else [ sdl3 ]);
# }
#
#   home-manager.lib.homeManagerConfiguration
# {
#   pkgs = pkgs;
#   modules = [{
#     homebrew = {
#       enable = true;
#       brews = [ "sdl3" ];
#     };
#   }];
# }
#
