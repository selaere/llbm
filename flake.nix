{
inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  flake-utils.url = "github:numtide/flake-utils";
  easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";
  #bqnlibs.url = "https://github.com/mlochbaum/bqn-libs";
  #bqnlibs.flake = false;
};

outputs = { nixpkgs, flake-utils, easy-purescript-nix, ... }:
  flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      easy-ps = easy-purescript-nix.packages.${system};
    in { devShells.default = pkgs.mkShell {
      name = "purescript-custom-shell";
      buildInputs = [
        easy-ps.purs-0_15_15
        easy-ps.spago
        easy-ps.purs-backend-es
        #easy-ps.purescript-language-server
        #easy-ps.purs-tidy
        #pkgs.nodejs-18_x
        pkgs.esbuild
        pkgs.cbqn
      ];
      shellHook = ''
        source <(spago --bash-completion-script `which spago`)
      '';
    };
  }
);
}
