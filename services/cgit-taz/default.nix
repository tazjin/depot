# This derivation configures a 'cgit' instance to serve repositories
# from a different source.
#
# In the first round this will just serve my GitHub repositories until
# I'm happy with the display.

{ pkgs, ... }:

with pkgs.third_party;

let
  cgitConfig = writeText "cgitrc" ''
    virtual-root=/cgit.cgi/

    repo.url=depot
    repo.path=/home/tazjin/depot/.git
    repo.desc=tazjin's personal monorepo
  '';
  cgitPatch = writeText "cgit_config.patch" ''
    diff --git a/Makefile b/Makefile
    index 05ea71f..0df886e 100644
    --- a/Makefile
    +++ b/Makefile
    @@ -4,7 +4,7 @@ CGIT_VERSION = v1.2.1
     CGIT_SCRIPT_NAME = cgit.cgi
     CGIT_SCRIPT_PATH = /var/www/htdocs/cgit
     CGIT_DATA_PATH = $(CGIT_SCRIPT_PATH)
    -CGIT_CONFIG = /etc/cgitrc
    +CGIT_CONFIG = ${cgitConfig}
     CACHE_ROOT = /var/cache/cgit
     prefix = /usr/local
     libdir = $(prefix)/lib
  '';
  cgitWithConfig = cgit.overrideAttrs(old: {
    patches = old.patches ++ [ cgitPatch ];
  });
  thttpdConfig = writeText "thttpd.conf" ''
    port=8080
    dir=${cgitWithConfig}/cgit
    nochroot
    novhost
    logfile=/dev/stdout
    cgipat=**.cgi
  '';
  # Patched version of thttpd that serves cgit.cgi as the index
  thttpdCgit = thttpd.overrideAttrs(old: {
    patches = [ ./cgit_idx.patch ];
  });
in writeShellScriptBin "cgit-launch" ''
  exec ${thttpdCgit}/bin/thttpd -D -C ${thttpdConfig}
# ''
