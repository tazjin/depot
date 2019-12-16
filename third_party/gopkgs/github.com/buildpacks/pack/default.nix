{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/buildpacks/pack";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "buildpacks";
    repo = "pack";
    rev = "508158aa547ed2f580aa8eee808f035bdc8b1806";
    sha256 = "0zqmf02b13nrwkyn05jmcxpxwz6r1aa0dh05pwc1pzpw1w4qxad3";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".BurntSushi.toml
    gopkgs."github.com".Masterminds.semver
    gopkgs."github.com".apex.log
    gopkgs."github.com".buildpacks.imgutil
    gopkgs."github.com".buildpacks.imgutil.local
    gopkgs."github.com".buildpacks.imgutil.remote
    gopkgs."github.com".buildpacks.lifecycle
    gopkgs."github.com".buildpacks.lifecycle.auth
    gopkgs."github.com".docker.docker.api.types
    gopkgs."github.com".docker.docker.api.types.container
    gopkgs."github.com".docker.docker.client
    gopkgs."github.com".docker.docker.pkg.ioutils
    gopkgs."github.com".docker.docker.pkg.jsonmessage
    gopkgs."github.com".docker.docker.pkg.stdcopy
    gopkgs."github.com".docker.go-connections.nat
    gopkgs."github.com".google.go-containerregistry.pkg.authn
    gopkgs."github.com".google.go-containerregistry.pkg.name
    gopkgs."github.com".google.go-containerregistry.pkg.v1
    gopkgs."github.com".google.go-containerregistry.pkg.v1.tarball
    gopkgs."github.com".heroku.color
    gopkgs."github.com".mitchellh.ioprogress
    gopkgs."github.com".pkg.errors
    gopkgs."github.com".spf13.cobra
  ];
}
