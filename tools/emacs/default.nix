# This file builds an Emacs pre-configured with the packages I need
# and my personal Emacs configuration.

{ pkgs, ... }:

with pkgs;
with third_party.emacsPackagesNg;
with third_party.emacs;

let
  localPackages = pkgs.tools.emacs-pkgs;
  emacsWithPackages = (third_party.emacsPackagesNgGen third_party.emacs26).emacsWithPackages;

  identity = x: x;
  tazjinsEmacs = pkgfun: (emacsWithPackages(epkgs: pkgfun(
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
    notmuch # this comes from pkgs.third_party
    paredit
    password-store
    pg
    prescient
    racket-mode
    rainbow-delimiters
    refine
    restclient
    request
    sly
    string-edit
    swiper
    telephone-line
    terraform-mode
    toml-mode
    transient
    use-package
    uuidgen
    vterm
    web-mode
    websocket
    which-key
    xelb
    yaml-mode
  ]) ++

  # Custom packages
  (with localPackages; [
    carp-mode
    dottime
    nix-util
    term-switcher
  ]))));
in lib.fix(self: l: f: third_party.writeShellScriptBin "tazjins-emacs" ''
  exec ${tazjinsEmacs f}/bin/emacs \
    --debug-init \
    --no-site-file \
    --no-site-lisp \
    --no-init-file \
    --directory ${./config} ${if l != null then "--directory ${l}" else ""} \
    --eval "(require 'init)" $@
  '' // {
    # Call overrideEmacs with a function (pkgs -> pkgs) to modify the
    # packages that should be included in this Emacs distribution.
    overrideEmacs = f': self l f';

    # Call withLocalConfig with the path to a *folder* containing a
    # `local.el` which provides local system configuration.
    withLocalConfig = confDir: self confDir f;

    # Build a derivation that uses the specified local Emacs (i.e.
    # built outside of Nix) instead
    withLocalEmacs = emacsBin: third_party.writeShellScriptBin "tazjins-emacs" ''
      export EMACSLOADPATH="${(tazjinsEmacs f).deps}/share/emacs/site-lisp:"
      exec ${emacsBin} \
        --debug-init \
        --no-site-file \
        --no-site-lisp \
        --no-init-file \
        --directory ${./config} \
        ${if l != null then "--directory ${l}" else ""} \
        --eval "(require 'init)" $@
    '';
  }) null identity
