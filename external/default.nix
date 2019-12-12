# Copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0
{ pkgs, program, package }:

let
  inherit (builtins) elemAt foldl' fromJSON head length readFile replaceStrings tail throw;
  inherit (pkgs) lib runCommand go jq ripgrep;

  pathToName = p: replaceStrings ["/"] ["_"] (toString p);

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

  mkset = path: value:
    if path == [] then { gopkg = value; }
    else { "${head path}" = mkset (tail path) value; };

  last = l: elemAt l ((length l) - 1);

  toPackage = self: src: path: entry:
    let
      args = {
        srcs = map (f: src + ("/" + f)) entry.files;
        deps = map (d: lib.attrByPath (d ++ [ "gopkg" ]) (
          throw "missing local dependency '${lib.concatStringsSep "." d}' in '${path}'"
        ) self) entry.localDeps;
      };
      libArgs = args // {
        name = pathToName entry.name;
        path = lib.concatStringsSep "/" ([ path ] ++ entry.locator);
      };
      binArgs = args // {
        name = last ([ path ] ++ entry.locator);
      };
    in if entry.isCommand then (program binArgs) else (package libArgs);

in { src, path, deps ? [] }: let
  name = pathToName path;
  analysisOutput = runCommand "${name}-structure.json" {} ''
    ${analyser}/bin/analyser -path ${path} -source ${src} > $out
  '';
  analysis = fromJSON (readFile analysisOutput);
in lib.fix(self: foldl' lib.recursiveUpdate {} (
  map (entry: mkset entry.locator (toPackage self src path entry)) analysis
))
