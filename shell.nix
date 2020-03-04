let
  sysPkg = import <nixpkgs> { };
  pinnedPkg = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "19.09";
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  };
  pkgs = import pinnedPkg {};
  stdenv = pkgs.stdenv;

in stdenv.mkDerivation {
  name = "env";
  buildInputs = [ pkgs.gnumake
                  pkgs.erlang
                ];
}
