{
  description = "Event Calendar Elm Project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv/latest";
    mkElmDerivation.url = "github:jeslie0/mkElmDerivation";
  };

  outputs = { self, nixpkgs, devenv, mkElmDerivation, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ mkElmDerivation.overlays.mkElmDerivation ];
      };
    in
    {
      packages.${system}.default = pkgs.mkElmDerivation {
        name = "event-calendar";
        src = ./.;
        outputJavaScript = true;
      };

      devShells.${system}.default = devenv.lib.mkShell {
        inherit inputs pkgs;
        modules = [
          ./devenv.nix
        ];
      };
    };
}
