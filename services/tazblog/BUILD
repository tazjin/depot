# Set all target’s visibility in this package to "public".
package(default_visibility = ["//visibility:public"])

# Load `rules_haskell` rules.
load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_binary",
    "haskell_library",
    "haskell_toolchain_library",
)

# Include required external libraries. These are added to the compiler's
# environment by Nix.
haskell_toolchain_library(name = "acid-state")
haskell_toolchain_library(name = "base")
haskell_toolchain_library(name = "base64-bytestring")
haskell_toolchain_library(name = "blaze-html")
haskell_toolchain_library(name = "bytestring")
haskell_toolchain_library(name = "containers")
haskell_toolchain_library(name = "cryptohash")
haskell_toolchain_library(name = "hamlet")
haskell_toolchain_library(name = "happstack-server")
haskell_toolchain_library(name = "ixset")
haskell_toolchain_library(name = "markdown")
haskell_toolchain_library(name = "mtl")
haskell_toolchain_library(name = "network")
haskell_toolchain_library(name = "network-uri")
haskell_toolchain_library(name = "rss")
haskell_toolchain_library(name = "safecopy")
haskell_toolchain_library(name = "shakespeare")
haskell_toolchain_library(name = "text")
haskell_toolchain_library(name = "time")
haskell_toolchain_library(name = "options")

haskell_library(
    name = "tazblog-lib",
    src_strip_prefix = "src",
    srcs = glob(['src/*.hs']),
    deps = [
        ":acid-state",
        ":base",
        ":base64-bytestring",
        ":blaze-html",
        ":bytestring",
        ":containers",
        ":cryptohash",
        ":hamlet",
        ":happstack-server",
        ":ixset",
        ":markdown",
        ":mtl",
        ":network",
        ":network-uri",
        ":rss",
        ":safecopy",
        ":shakespeare",
        ":text",
        ":time",
    ],
)

# Primary blog server component
haskell_binary(
    name = "tazblog",
    srcs = [":blog/Main.hs"],
    deps = [
        ":acid-state",
        ":base",
        ":network",
        ":options",
        ":tazblog-lib",
    ],
)

# Blog database server component
haskell_binary(
    name = "tazblog-db",
    srcs = [":db/Main.hs"],
    deps = [
        ":base",
        ":acid-state",
        ":network",
        ":options",
        ":tazblog-lib",
    ],
)
