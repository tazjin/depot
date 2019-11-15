path: { pkgs, ... } @ args:

let
  inherit (builtins)
    attrNames
    attrValues
    filter
    head
    isString
    listToAttrs
    map
    match
    readDir
    tail
    toPath
    toString;

  zipAttrs = names: values:
    if (names == []) || (values == [])
    then []
    else [{
      name = head names;
      value = head values;
    }] ++ zipAttrs (tail names) (tail values);

  attrsToList = attrs: zipAttrs (attrNames attrs) (attrValues attrs);

  isFile = s: s == "regular";
  isDir = s: s == "directory";

  joinPath = p: f: toPath ((toString p) + "/" + f);

  isNixFile = file:
    let res = match "(.*)\.nix" file;
    in if res == null then null else head res;

  filterNixFiles = dir:
    let files = filter (e: isFile e.value) dir;
        nixFiles = map (f: {
          # Name and value are intentionally flipped to get the
          # correct attribute set structure back out
          name = isNixFile f.name;
          value = f.name; # i.e. the path
        }) files;
    in filter (f: isString f.name) nixFiles;

  traverse = path: dir:
    let nixFiles = filterNixFiles dir;
        imported = map (f: {
          inherit (f) name;
          value = import (joinPath path f.value) args;
        }) nixFiles;
        dirs = map (d: {
          inherit (d) name;
          value = readTree (joinPath path d.name);
        }) (filter (e: isDir e.value) dir);
    in listToAttrs (imported ++ dirs);

  importOr = path: dir: f:
    if dir ? "default.nix"
      then import path args
      else f path (attrsToList dir);

  readTree = path: importOr path (readDir path) traverse;
in readTree path
