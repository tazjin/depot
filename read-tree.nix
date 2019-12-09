args: initPath:

let
  inherit (builtins)
    attrNames
    baseNameOf
    filter
    head
    length
    listToAttrs
    map
    match
    readDir;

  argsWithPath = parts: args // {
    locatedAt = parts;
  };

  # The marker is added to everything that was imported directly by
  # readTree.
  marker = { __readTree = true; };

  nixFileName = file:
    let res = match "(.*)\.nix" file;
    in if res == null then null else head res;

  readTree = path: parts:
    let
      dir = readDir path;
      self = (import path (argsWithPath parts)) // marker;
      joinChild = c: path + ("/" + c);

      # Import non-empty subdirectories
      filterDir = f: dir."${f}" == "directory";
      children = map (c: {
        name = c;
        value = readTree (joinChild c) (parts ++ [ c ]);
      }) (filter filterDir (attrNames dir));

      # Import Nix files
      nixFiles = filter (f: f != null) (map nixFileName (attrNames dir));
      nixChildren = map (c: let p = joinChild (c + ".nix"); in {
        name = c;
        value = (import p (argsWithPath (parts ++ [ c ]))) // marker;
      }) nixFiles;
    in if dir ? "default.nix"
      then self // (listToAttrs children)
      else listToAttrs (nixChildren ++ children);
in readTree initPath [ (baseNameOf initPath) ]
