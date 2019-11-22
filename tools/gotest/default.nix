# This file demonstrates how to make use of pkgs.buildGo.
#
# It introduces libraries and protobuf support, however gRPC support
# is not yet included.
#
# From the root of this repository this example can be built with
# `nix-build -A tools.gotest`
{ pkgs, ... }:

let
  inherit (pkgs) buildGo;

  somelib = buildGo.package {
    name = "somelib";
    srcs = [ ./lib.go ];
  };

  someproto = buildGo.proto {
    name = "someproto";
    proto = ./test.proto;
  };

in buildGo.program {
  name = "gotest";
  srcs = [ ./main.go ];
  deps = [ somelib someproto ];
} // { meta.enableCI = true; }
