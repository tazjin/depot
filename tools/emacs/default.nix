# This file builds an Emacs pre-configured with the packages I need
# and my personal Emacs configuration.

{ pkgs, ... }:

with pkgs;
with third_party.emacsPackagesNg;
with third_party.emacs;

let
  localPackages = pkgs.tools.emacs-pkgs;
  emacsWithPackages = (third_party.emacsPackagesNgGen third_party.emacs26).emacsWithPackages;

  tazjinsEmacs = (emacsWithPackages(epkgs:
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
  [ carp-mode localPackages.dottime ]
  ));
in third_party.writeShellScriptBin "tazjins-emacs" ''
  exec ${tazjinsEmacs}/bin/emacs \
    --debug-init \
    --no-site-file \
    --no-site-lisp \
    --no-init-file \
    --directory ${./config} \
    --eval "(require 'init)" $@
''
