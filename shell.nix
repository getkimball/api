let
  sysPkg = import <nixpkgs> { };
  releasedPkgs = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "20.03";
    sha256 = "12353i02hrdfa6kcla5h1q3j50mx39fchva7z7l32pk699nla4hi";
  };
  pinnedPkgs = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "1ddbd8ce8a6cde8c6c72fc305fed1f6dd4e0d24c";
    sha256 = "032zr8862r3cfl1f35qmy12b4f2al49qq66ls5pyigr4v4b0ykvy";
  };

  released_pkgs = import pinnedPkgs {};
  pinned_pkgs = import pinnedPkgs {};
  stdenv = released_pkgs.stdenv;

in stdenv.mkDerivation {
  name = "env";
  buildInputs = [ released_pkgs.gnumake
                  released_pkgs.erlang
                  pinned_pkgs.aws-iam-authenticator
                  pinned_pkgs.awscli
                  pinned_pkgs.kubectl
                  pinned_pkgs.kubectx
                  pinned_pkgs.kubernetes-helm
                  pinned_pkgs.kustomize
                  pinned_pkgs.nodejs-14_x
                ];
  shellHook = ''
            npm install
  '';

}
