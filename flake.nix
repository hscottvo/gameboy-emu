{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = { nixpkgs, ... }:
    let
      mkShell = pkgs: cfg: pkgs.mkShell (cfg // {
        packages = cfg.packages or [ ] ++ [
          pkgs.nil
          pkgs.cargo
          pkgs.rustc
          pkgs.clippy
          pkgs.rust-analyzer
          pkgs.rustfmt
          pkgs.libiconv
          pkgs.ffmpeg
        ];
      });
    in
    {
      devShells.x86_64-linux.default =
        let
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
        in
        mkShell pkgs {
          LD_LIBRARY_PATH = "${pkgs.xorg.libX11}/lib:${pkgs.xorg.libXcursor}/lib";
          shellHook = ''
            exec zsh
          '';
        };
      devShells.aarch64-darwin.default =
        let
          pkgs = nixpkgs.legacyPackages.aarch64-darwin;
        in
        mkShell pkgs {
          packages = [
            pkgs.darwin.apple_sdk.frameworks.Cocoa
            pkgs.darwin.apple_sdk.frameworks.MetalKit
          ];
          shellHook = ''
            export DYLD_LIBRARY_PATH=${pkgs.rustc}/lib
            exec zsh
          '';
        };
    };
}

