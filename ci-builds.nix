# This file is invoked by the CI build and recursively filters the
# package set for attributes that should be built automatically.
#
# Packages can be opted-in to being built by CI by setting
# `meta.enableCI = true`.
#
# TODO(tazjin): Actually implement the above.

let
  pkgs = import ./default.nix {};
in with pkgs; [
  services.tazblog
  services.nixcon-demo
  tools.kms_pass
  tools.blog_cli
]

