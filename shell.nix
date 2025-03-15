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
