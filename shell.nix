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
    rev = "2b8a7711f63a701d80e1b2047e4e39e00f298165";
    sha256 = "0z5m47pyc2pyv9nnvf3rrgxj77zlh8zgaif16gicnz02h6llggk3";
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

                  pinned_pkgs.heroku

                  pinned_pkgs.python38
                  pinned_pkgs.python38Packages.pip
                  pinned_pkgs.python38Packages.virtualenv
                  pinned_pkgs.python38Packages.numpy
                  pinned_pkgs.wget
                ];
  shellHook = ''
            set -e
            alias pip="PIP_PREFIX='$(pwd)/_build/pip_packages' \pip"
            export PYTHONPATH="$(pwd)/_build/pip_packages/lib/python3.8/site-packages:$PYTHONPATH"
            unset SOURCE_DATE_EPOCH
            virtualenv .venv
            source .venv/bin/activate
            python3 -m pip install -r pymodels/requirements.txt
            source .env

            npm install
  '';

}
