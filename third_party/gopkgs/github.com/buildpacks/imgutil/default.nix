{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/buildpacks/imgutil";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "buildpacks";
    repo = "imgutil";
    rev = "dc184e0d403baee6564954e619d588754105e791";
    sha256 = "1i6knq0cd28k06spkryv76wn3wyaafi675k3q4fazhpk9njb9nfl";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".docker.docker.api.types
    gopkgs."github.com".docker.docker.api.types.container
    gopkgs."github.com".docker.docker.client
    gopkgs."github.com".google.go-containerregistry.pkg.name
    gopkgs."github.com".google.go-containerregistry.pkg.authn
    gopkgs."github.com".google.go-containerregistry.pkg.v1
    gopkgs."github.com".google.go-containerregistry.pkg.v1.mutate
    gopkgs."github.com".google.go-containerregistry.pkg.v1.random
    gopkgs."github.com".google.go-containerregistry.pkg.v1.remote
    gopkgs."github.com".google.go-containerregistry.pkg.v1.tarball
    gopkgs."github.com".google.go-containerregistry.pkg.v1.types
    gopkgs."github.com".google.go-containerregistry.pkg.v1.remote.transport
    gopkgs."github.com".pkg.errors
    gopkgs."golang.org".x.sync.singleflight
  ];
}
