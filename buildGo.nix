# Copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0
#
# buildGo provides Nix functions to build Go packages in the style of Bazel's
# rules_go.

{ pkgs ? import <nixpkgs> {}
, ... }:

let
  inherit (builtins)
    attrNames
    baseNameOf
    dirOf
    elemAt
    filter
    map
    match
    readDir
    replaceStrings;

  inherit (pkgs) lib go runCommand fetchFromGitHub protobuf symlinkJoin;

  # Helpers for low-level Go compiler invocations
  spaceOut = lib.concatStringsSep " ";

  includeDepSrc = dep: "-I ${dep}";
  includeSources = deps: spaceOut (map includeDepSrc deps);

  includeDepLib = dep: "-L ${dep}";
  includeLibs = deps: spaceOut (map includeDepLib deps);

  srcBasename = src: elemAt (match "([a-z0-9]{32}\-)?(.*\.go)" (baseNameOf src)) 1;
  srcCopy = path: src: "cp ${src} $out/${path}/${srcBasename src}";
  srcList = path: srcs: lib.concatStringsSep "\n" (map (srcCopy path) srcs);

  isGoFile = f: (match ".*\.go" f) != null;
  isGoTest = f: (match ".*_test\.go" f) != null;
  goFileFilter = k: v: (v == "regular") && (isGoFile k) && (!isGoTest k);
  goFilesIn = dir:
    let files = readDir dir;
        goFiles = filter (f: goFileFilter f files."${f}") (attrNames files);
    in map (f: dir + "/" + f) goFiles;

  allDeps = deps: lib.unique (lib.flatten (deps ++ (map (d: d.goDeps) deps)));

  xFlags = x_defs: spaceOut (map (k: "-X ${k}=${x_defs."${k}"}") (attrNames x_defs));

  pathToName = p: replaceStrings ["/"] ["_"] (toString p);

  # High-level build functions

  # Build a Go program out of the specified files and dependencies.
  program = { name, srcs, deps ? [], x_defs ? {} }:
  let uniqueDeps = allDeps deps;
  in runCommand name {} ''
    ${go}/bin/go tool compile -o ${name}.a -trimpath=$PWD -trimpath=${go} ${includeSources uniqueDeps} ${spaceOut srcs}
    mkdir -p $out/bin
    ${go}/bin/go tool link -o $out/bin/${name} -buildid nix ${xFlags x_defs} ${includeLibs uniqueDeps} ${name}.a
  '';

  # Build a Go library assembled out of the specified files.
  #
  # This outputs both the sources and compiled binary, as both are
  # needed when downstream packages depend on it.
  package = { name, srcs, deps ? [], path ? name }:
  let uniqueDeps = allDeps deps;
  in (runCommand "golib-${name}" {} ''
    mkdir -p $out/${path}
    ${srcList path (map (s: "${s}") srcs)}
    ${go}/bin/go tool compile -o $out/${path}.a -trimpath=$PWD -trimpath=${go} -p ${path} ${includeSources uniqueDeps} ${spaceOut srcs}
  '') // { goDeps = uniqueDeps; };

  # Build a Go library out of the specified protobuf definition.
  proto = { name, proto, path ? name, extraDeps ? [] }: package {
    inherit name path;
    deps = [ protoLibs.goProto ] ++ extraDeps;
    srcs = lib.singleton (runCommand "goproto-${name}.pb.go" {} ''
      cp ${proto} ${baseNameOf proto}
      ${protobuf}/bin/protoc --plugin=${protoLibs.goProto}/bin/protoc-gen-go \
        --go_out=plugins=grpc,import_path=${baseNameOf path}:. ${baseNameOf proto}
      mv *.pb.go $out
    '');
  };

  # Build a Go library out of the specified gRPC definition.
  grpc = args: proto (args // { extraDeps = [ protoLibs.goGrpc ]; });

  # Build an externally defined Go library using `go build` itself.
  #
  # Libraries built this way can be included in any standard buildGo
  # build.
  #
  # Contrary to other functions, `src` is expected to point at a
  # single directory containing the root of the external library.
  external = { path, src, deps ? [], srcOnly ? false, targets ? [ "..." ] }:
    let
      name = pathToName path;
      uniqueDeps = allDeps deps;
      srcDir = runCommand "goext-src-${name}" {} ''
        mkdir -p $out/${dirOf path}
        cp -r ${src} $out/${dirOf path}/${baseNameOf path}
      '';
      gopathSrc = symlinkJoin {
        name = "gopath-${name}";
        paths = uniqueDeps ++ [ srcDir ];
      };
      gopathPkg = runCommand "goext-pkg-${name}" {} ''
        mkdir -p gopath $out
        export GOPATH=$PWD/gopath
        ln -s ${gopathSrc} gopath/src
        ${go}/bin/go install ${spaceOut (map (t: path + "/" + t) targets)}

        if [[ -d gopath/pkg/linux_amd64 ]]; then
          echo "Installing Go packages for ${path}"
          mv gopath/pkg/linux_amd64/* $out
        fi

        if [[ -d gopath/bin ]]; then
          echo "Installing Go binaries for ${path}"
          mv gopath/bin $out/bin
        fi
      '';
    in (if srcOnly then gopathSrc else symlinkJoin {
      name = "goext-${name}";
      paths = [ gopathSrc gopathPkg ];
    }) // { goDeps = uniqueDeps; };

  protoLibs = import ./proto.nix {
    inherit external;
  };
in {
  # Only the high-level builder functions are exposed
  inherit program package proto grpc external;
}
