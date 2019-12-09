# TODO(tazjin): avoid {} by only calling functions *after* checking what they are

args: initPath:

let
  inherit (builtins)
    attrNames
    filter
    head
    isString
    length
    listToAttrs
    map
    match
    readDir
    split
    tail
    toString;

  attrsToList = attrs: map (name: {
    inherit name;
    value = attrs."${name}";
  }) (attrNames attrs);

  isFile = s: s == "regular";
  isDir = s: s == "directory";

  joinPath = p: f: p + ("/" + f);

  isNixFile = file:
    let res = match "(.*)\.nix" file;
    in if res == null then null else head res;

  filterNixFiles = dir:
    let files = filter (e: isFile e.value && e.name != "default.nix") dir;
        nixFiles = map (f: {
          # Name and value are intentionally flipped to get the
          # correct attribute set structure back out
          name = isNixFile f.name;
          value = f.name; # i.e. the path
        }) files;
    in filter (f: isString f.name) nixFiles;

  # Some packages require that their position in the tree is passed in
  # as an argument. To do this the root directory (i.e. $PWD during
  # imports) is chopped off the front of the path components in
  # imports.
  pathParts = p: tail (filter isString (split "/" (toString p)));
  initLen = length (pathParts ./.);
  drop = n: l:
    if n == 0
      then l
      else if l == []
        then []
        else drop (n - 1) (tail l);

  argsWithPath = args: parts: args // {
    locatedAt = drop initLen parts;
  };

  traverse = path: dir:
    let nixFiles = filterNixFiles dir;
        imported = map (f: {
          inherit (f) name;
          value = import (joinPath path f.value) (argsWithPath args (pathParts path));
        }) nixFiles;
        dirs = map (d: {
          inherit (d) name;
          value = readTree (joinPath path d.name);
        }) (filter (e: isDir e.value) dir);
    in listToAttrs (imported ++ dirs);

  importOr = path: dir: f:
    let
      allContents = f path (attrsToList dir);
      dirOnlyContents = f path (filter (f: f.value == "directory") (attrsToList dir));
    in if dir ? "default.nix"
      then import path (argsWithPath args (pathParts path))
        // { __treeChildren = true; } # used downstream for traversals
        // dirOnlyContents
      else allContents;

  readTree = path: importOr path (readDir path) traverse;
in readTree initPath
