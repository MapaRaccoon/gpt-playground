{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: with pkgs.haskell.lib; {
          gpt-playground = self.callCabal2nix "gpt-playground" ./. {};
        };
      };
    in
    {
      packages.default = haskellPackages.gpt-playground;

      devShells.default = haskellPackages.shellFor {
        packages = ps: [ ps.gpt-playground ];
        withHoogle = true;
        nativeBuildInputs = with haskellPackages; [
          cabal-install
          fourmolu
          haskell-language-server
        ];
      };
    }
  );
}
