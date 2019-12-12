# Copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0
{ runCommand, go, jq, ripgrep, program }:

let
  # Collect all non-vendored dependencies from the Go standard library
  # into a file that can be used to filter them out when processing
  # dependencies.
  stdlibPackages = runCommand "stdlib-pkgs.json" {} ''
    export GOPATH=/dev/null
    ${go}/bin/go list all | \
      ${ripgrep}/bin/rg -v 'vendor' | \
      ${jq}/bin/jq -R '.' | \
      ${jq}/bin/jq -c -s 'map({key: ., value: true}) | from_entries' \
      > $out
  '';

  analyser = program {
    name = "analyser";

    srcs = [
      ./main.go
    ];

    x_defs = {
      "main.stdlibList" = "${stdlibPackages}";
    };
  };
in analyser
