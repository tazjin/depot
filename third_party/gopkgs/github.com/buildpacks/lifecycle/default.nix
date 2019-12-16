{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/buildpacks/lifecycle";
  src = pkgs.third_party.fetchFromGitHub {
    owner = "buildpacks";
    repo = "lifecycle";
    rev = "b9dc1bd64e7f9c001139c80cbfa884951599451e";
    sha256 = "0gr7qh74g7cp0mbj6dmn48j26sk4cra2dbipm5hvb8bmhmz1q106";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".BurntSushi.toml
    gopkgs."github.com".apex.log
    gopkgs."github.com".buildpacks.imgutil
    gopkgs."github.com".buildpacks.imgutil.local
    gopkgs."github.com".buildpacks.imgutil.remote
    gopkgs."github.com".docker.docker.client
    gopkgs."github.com".google.go-containerregistry.pkg.authn
    gopkgs."github.com".google.go-containerregistry.pkg.name
    gopkgs."github.com".heroku.color
    gopkgs."github.com".pkg.errors
    gopkgs."golang.org".x.sync.errgroup
  ];
}
