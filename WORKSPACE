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

load(
  "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
  "nixpkgs_cc_configure",
  "nixpkgs_package",
)

nixpkgs_cc_configure(
  repositories = { "nixpkgs": "default.nix" },
)

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

# Register a Haskell toolchain with all required external
# dependencies.
#
# All dependencies need to be set up in thirdParty.ghc in default.nix
haskell_register_ghc_nixpkgs(
    version = "8.6.5",
    repositories = { "nixpkgs": "default.nix" },
    attribute_path = "thirdParty.ghc",
)
