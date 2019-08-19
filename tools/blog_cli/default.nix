{ buildGoPackage }:

buildGoPackage {
  name = "blog_cli";
  goPackagePath = "github.com/tazjin/personal/blog_cli";
  src = ./.;
  goDeps = ./deps.nix;
}
