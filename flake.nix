{

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: {
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
          ];
          shellHook = "exec zsh --login";
        };
      };
  };
}
