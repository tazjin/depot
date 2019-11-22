# buildGo provides Nix functions to build Go packages in the style of Bazel's
# rules_go.
#
# TODO(tazjin): Go through Bazel rules_go options and implement corresponding flags
# TODO(tazjin): Refactor to include /golang/protobuf/descriptor in goProto deps
# TODO(tazjin): Find a way to expose documentation (esp. for generated stuff)

{ pkgs, ... }@args:

let
  inherit (builtins)
    attrNames
    baseNameOf
    elemAt
    filter
    map
    match
    readDir
    replaceStrings
    toPath;

  inherit (pkgs) bash lib go runCommand fetchFromGitHub protobuf;

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

  # Build a Go library out of the specified protobuf definition.
  proto = { name, proto, path ? name, protocFlags ? "", extraDeps ? [] }: package {
    inherit name path;
    deps = [ goProto ] ++ extraDeps;
    srcs = lib.singleton (runCommand "goproto-${name}.pb.go" {} ''
      cp ${proto} ${baseNameOf proto}
      ${protobuf}/bin/protoc --plugin=${protocGo}/bin/protoc-gen-go \
        --go_out=${protocFlags}import_path=${baseNameOf path}:. ${baseNameOf proto}
      mv *.pb.go $out
    '');
  };

  # Protobuf & gRPC integration requires these dependencies:
  proto-go-src = fetchFromGitHub {
    owner = "golang";
    repo = "protobuf";
    rev = "ed6926b37a637426117ccab59282c3839528a700";
    sha256 = "0fynqrim022x9xi2bivkw19npbz4316v4yr7mb677s9s36z4dc4h";
  };

  protoPart = path: deps: package {
    inherit deps;
    name = replaceStrings ["/"] ["_"] path;
    path = "github.com/golang/protobuf/${path}";
    srcs = goFilesIn (toPath "${proto-go-src}/${path}");
  };

  goProto =
    let
      protobuf = package {
        name = "protobuf";
        path = "github.com/golang/protobuf/proto";
        # TODO(tazjin): How does this build toggle work?
        srcs = filter
          (f: (match "(.*)/pointer_reflect.go" f) == null)
          (goFilesIn (toPath "${proto-go-src}/proto"));
      };
      type = name: protoPart "ptypes/${name}" [ protobuf ];
      descriptor = protoPart "descriptor" [ protobuf ];
      ptypes = package {
        name = "ptypes";
        path = "github.com/golang/protobuf/ptypes";
        srcs = goFilesIn (toPath "${proto-go-src}/ptypes");
        deps = map type [
          "any"
          "duration"
          "empty"
          "struct"
          "timestamp"
          "wrappers"
        ];
      };
    in protobuf // { goDeps = allDeps (protobuf.goDeps ++ [ ptypes ]); };

  protocDescriptor = (protoPart "protoc-gen-go/descriptor" [ goProto ]);
  protocGo =
    let
      generator = protoPart "protoc-gen-go/generator" [
        (protoPart "protoc-gen-go/generator/internal/remap" [])
        (protoPart "protoc-gen-go/plugin" [ protocDescriptor ])
      ];
      grpc = protoPart "protoc-gen-go/grpc" [ generator ];
    in program {
      name = "protoc-gen-go";
      deps = [ goProto grpc generator ];
      srcs = filter
        (f: (match "(.*)/doc.go" f) == null)
        (goFilesIn (toPath "${proto-go-src}/protoc-gen-go"));
    };
in {
  # Only the high-level builder functions are exposed
  inherit program package proto;
}
