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
    elemAt
    filter
    map
    match
    readDir;

  inherit (pkgs) lib go runCommand;

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

  # High-level build functions

  # Build a Go program out of the specified files and dependencies.
  program = { name, srcs, deps ? [] }:
  let uniqueDeps = allDeps deps;
  in runCommand name {} ''
    ${go}/bin/go tool compile -o ${name}.a -trimpath=$PWD -trimpath=${go} ${includeSources uniqueDeps} ${spaceOut srcs}
    mkdir -p $out/bin
    ${go}/bin/go tool link -o $out/bin/${name} -buildid nix ${includeLibs uniqueDeps} ${name}.a
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
in {
  # Only the high-level builder functions are exposed
  inherit program package;
}
