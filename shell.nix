let
  sysPkg = import <nixpkgs> { };
  releasedPkgs = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "20.09";
    sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
  };
  stdenv = released_pkgs.stdenv;

in stdenv.mkDerivation {
  name = "env";
  buildInputs = [ released_pkgs.gnumake
                  released_pkgs.erlangR23
                  released_pkgs.aws-iam-authenticator
                  released_pkgs.awscli
                  released_pkgs.kubectl
                  released_pkgs.kubectx
                  released_pkgs.kubernetes-helm
                  released_pkgs.kustomize
                  released_pkgs.nodejs-14_x

                  released_pkgs.heroku

                  released_pkgs.python38
                  released_pkgs.python38Packages.pip
                  released_pkgs.python38Packages.virtualenv
                  released_pkgs.python38Packages.numpy
                  released_pkgs.wget
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
