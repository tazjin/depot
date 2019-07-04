# -*- mode: bazel; -*-
#
# This workspace configuration loads all Bazel rule sets that need to
# be available in the entire repository.

workspace(name = "tazjin_monorepo")

# SECTION: Nix

local_repository(
  name = "io_tweag_rules_nixpkgs",
  path = "third_party/bazel/rules_nixpkgs",
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_local_repository", "nixpkgs_package")

# SECTION: Haskell

local_repository(
  name = "io_tweag_rules_haskell",
  path = "third_party/bazel/rules_haskell",
)

load(
    "@io_tweag_rules_haskell//haskell:repositories.bzl",
    "haskell_repositories"
)

haskell_repositories()

load(
    "@io_tweag_rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

haskell_register_ghc_nixpkgs(
    version = "8.6.4",
    repositories = { "nixpkgs": "default.nix" }
)
