# This file assembles a preconfigured Emacs with the dependencies that
# I need.
#
# It can either build Emacs itself (`-A complete`) or just the
# configuration (`-A config`). If the configuration is built
# separately (e.g. for work machines where Emacs itself is installed
# by other means) it is important that the versions of Emacs are kept
# in sync.

{ pkgs, ... }:

with pkgs;
with third_party.emacsPackagesNg;

let
  emacsWithPackages = (third_party.emacsPackagesNgGen third_party.emacs26).emacsWithPackages;

  carpMode = melpaBuild {
    pname = "carp-mode";
    ename = "carp-mode";
    version = "3.0";
    recipe = builtins.toFile "recipe" ''
      (carp-mode :fetcher github
                 :repo "carp-lang/carp"
                 :files ("emacs/*.el"))
    '';

    packageRequires = [ clojure-mode ];
    src = third_party.fetchFromGitHub {
      owner = "carp-lang";
      repo = "carp";
      rev = "6954642cadee730885717201c3180c7acfb1bfa9";
      sha256 = "1pz4x2qkwjbz789bwc6nkacrjpzlxawxhl2nv0xdp731y7q7xyk9";
    };
  };

  complete = (emacsWithPackages(epkgs:
  # Actual ELPA packages (the enlightened!)
  (with epkgs.elpaPackages; [
    ace-window
    avy
    pinentry
    rainbow-mode
    undo-tree
  ]) ++

  # MELPA packages:
  (with epkgs.melpaPackages; [
    browse-kill-ring
    cargo
    clojure-mode
    counsel
    counsel-notmuch
    dash-functional
    direnv
    dockerfile-mode
    # TODO: eglot removed until workspace-folders are supported (needed for gopls)
    # eglot
    elixir-mode
    elm-mode
    erlang
    exwm
    go-mode
    gruber-darker-theme
    haskell-mode
    ht
    hydra
    idle-highlight-mode
    intero
    ivy
    ivy-pass
    ivy-prescient
    jq-mode
    kotlin-mode
    lsp-mode
    magit
    markdown-toc
    multi-term
    multiple-cursors
    nginx-mode
    nix-mode
    paredit
    password-store
    pg
    notmuch # this comes from pkgs.third_party
    prescient
    racket-mode
    rainbow-delimiters
    restclient
    sly
    smartparens
    string-edit
    swiper
    telephone-line
    terraform-mode
    toml-mode
    transient
    use-package
    uuidgen
    web-mode
    websocket
    which-key
    xelb
    yaml-mode
  ]) ++

  # Custom packages
  [ carpMode ]
  ));
in {
  inherit complete;
  depsOnly = complete.deps;
}
