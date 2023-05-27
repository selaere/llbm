/*{

inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";
  bqnlibs.url = "https://github.com/mlochbaum/bqn-libs";
  bqnlibs.flake = false;
  data.url = (???)
};

description = ",leader lead board man?";

outputs = { self, nixpkgs, data}: {
  packages.x86_64-linux.default = */

{ data ? "" } :
  with import <nixpkgs> {system = "x86_64-linux";};
  pkgs.stdenv.mkDerivation {
    name = "llbm";
    src = ./.;
    buildInputs = with pkgs; [ cbqn elmPackages.elm ];
    elmapp = import nix/app.nix {};
    bqnlibs = builtins.fetchGit { url = "https://github.com/mlochbaum/bqn-libs"; };
    buildPhase = ''
      ln -s $bqnlibs include
      echo ${data}
      bqn process.bqn ${data}
    '';
    installPhase = ''
      mkdir -p $out
      cp -r {output.txt,"$elmapp/Main.min.js",index.html} $out
    '';

  }
  
/*};

}*/
