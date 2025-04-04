{

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: {
    devShells.aarch64-darwin =
      let
        pkgs = import nixpkgs { system = "aarch64-darwin"; };
      in
      {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cargo
            cargo-watch
            libwayland-dev
            lld
            rustc
            SDL
            sdl3
            wasm-pack
          ];
          shellHook = "exec zsh --login";
        };
      };
    devShells.x86_64-linux =
      let
        pkgs = import nixpkgs { system = "x86_64-linux"; };
      in
      {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cargo
            cargo-watch
            lld
            rustc
            SDL
            sdl3
            wasm-pack
            zsh

            pkg-config
            # rust-bin.nightly.latest.default

            xorg.libX11
            xorg.libXcursor
            xorg.libXrandr
            xorg.libXi
            xorg.libxcb
            libxkbcommon
            vulkan-loader
            wayland
          ];
          shellHook = ''
            exec zsh --login
          '';
          # export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${builtins.toString (pkgs.lib.makeLibraryPath buildInputs)}";
        };
      };
  };
}
