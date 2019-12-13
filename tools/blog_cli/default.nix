{ pkgs, ... }:

pkgs.buildGo.program {
  name = "blog_cli";
  srcs = [ ./main.go ];
  deps = with pkgs.third_party; [
    gopkgs."google.golang.org".api.dns.v1.gopkg
  ];
} // { meta.enableCI = true; }
