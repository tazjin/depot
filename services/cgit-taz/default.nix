# This derivation configures a 'cgit' instance to serve repositories
# from a different source.
#
# In the first round this will just serve my GitHub repositories until
# I'm happy with the display.

{ pkgs, ... }:

with pkgs.third_party;

let
  cgitConfig = writeText "cgitrc" ''
    # Global configuration
    virtual-root=/cgit.cgi/
    enable-http-clone=1

    # Repository configuration
    repo.url=depot
    repo.path=/git/depot
    repo.desc=tazjin's personal monorepo
    repo.owner=tazjin <tazjin@google.com>
    repo.clone-url=https://git.tazj.in ssh://source.developers.google.com:2022/p/tazjins-infrastructure/r/depot
  '';
  thttpdConfig = writeText "thttpd.conf" ''
    port=8080
    dir=${cgit}/cgit
    nochroot
    novhost
    logfile=/dev/stdout
    cgipat=**.cgi
  '';

  # Patched version of thttpd that serves cgit.cgi as the index and
  # sets the environment variable for pointing cgit at the correct
  # configuration.
  #
  # Things are done this way because recompilation of thttpd is much
  # faster than cgit and I don't want to wait long when iterating on
  # config.
  thttpdConfigPatch = writeText "thttpd_cgit_conf.patch" ''
    diff --git a/libhttpd.c b/libhttpd.c
    index c6b1622..eef4b73 100644
    --- a/libhttpd.c
    +++ b/libhttpd.c
    @@ -3055,4 +3055,6 @@ make_envp( httpd_conn* hc )

         envn = 0;
    +    // force cgit to load the correct configuration
    +    envp[envn++] = "CGIT_CONFIG=${cgitConfig}";
         envp[envn++] = build_env( "PATH=%s", CGI_PATH );
     #ifdef CGI_LD_LIBRARY_PATH
  '';
  thttpdCgit = thttpd.overrideAttrs(old: {
    patches = [ ./cgit_idx.patch thttpdConfigPatch ];
  });
in writeShellScriptBin "cgit-launch" ''
  # v0v
  gcloud config set core/custom_ca_certs_file ${cacert}/etc/ssl/certs/ca-bundle.crt

  # The role account that this container is running at in Kubernetes
  # has permission to clone the repository.
  ${google-cloud-sdk}/bin/gcloud source repos --project tazjins-infrastructure clone depot /git/depot

  exec ${thttpdCgit}/bin/thttpd -D -C ${thttpdConfig}
# ''
