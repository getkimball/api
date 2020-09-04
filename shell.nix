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
    rev = "c7e8da62257a8ff5537c3b3c43eb68228f51e5df";
    sha256 = "0b5qiywfccng1w6gxs045mhwfz623jw72m68g8viwh1g3p500k3y";
  };

  released_pkgs = import pinnedPkgs {};
  pinned_pkgs = import pinnedPkgs {};
  stdenv = released_pkgs.stdenv;

in stdenv.mkDerivation {
  name = "env";
  buildInputs = [ released_pkgs.gnumake
                  pinned_pkgs.erlangR23
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
