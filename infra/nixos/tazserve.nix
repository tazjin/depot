{ pkgs, config, ... }:

with pkgs; let blogSource = fetchgit {
  url = "https://git.tazj.in/tazjin/tazblog.git";
  sha256 = "0m745vb8k6slzdsld63rbfg583k70q3g6i5lz576sccalkg0r2l2";
  rev = "aeeb11f1b76729115c4db98f419cbcda1a0f7660";
};
tazblog = import ./tazblog { inherit blogSource; };
blog = tazblog.tazblog;
blogConfig = {
  enableACME = true;
  forceSSL = true;
  locations."/" = {
    proxyPass = "http://127.0.0.1:8000";
  };
};
gemma = import ./pkgs/gemma.nix { inherit pkgs; };
gemmaConfig = writeTextFile {
  name = "config.lisp";
  text = builtins.readFile ./gemma-config.lisp;
};
in {
  # Ensure that blog software is installed
  environment.systemPackages = [
    blog
    blogSource
  ];

  # Set up database unit
  systemd.services.tazblog-db =  {
    description           = "Database engine for Tazblog";
    script                = "${blog}/bin/tazblog-db";
    serviceConfig.restart = "always";
    wantedBy              = [ "multi-user.target" ];
  };

  # Set up blog unit
  systemd.services.tazblog = {
    description           = "Tazjin's blog engine";
    script                = "${blog}/bin/tazblog --resourceDir ${blogSource}/static";
    serviceConfig.restart = "always";
    requires              = [ "tazblog-db.service" ];
    wantedBy              = [ "multi-user.target" ];
  };

  # Set up Gogs
  services.gogs = {
    enable       = true;
    appName      = "Gogs: tazjin's private code";
    cookieSecure = true;
    domain       = "git.tazj.in";
    rootUrl      = "https://git.tazj.in/";
    extraConfig = ''
      [log]
      ROOT_PATH = /var/lib/gogs/log
    '';
  };

  # Set up Gemma
  systemd.services.gemma = {
    description           = "Recurring task tracking app";
    script                = "${gemma}/bin/gemma";
    serviceConfig.Restart = "always";
    wantedBy              = [ "multi-user.target" ];

    environment = {
      GEMMA_CONFIG = "${gemmaConfig}";
    };
  };

  # Set up reverse proxy
  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;

    # Blog!
    virtualHosts."tazj.in" = blogConfig;
    virtualHosts."www.tazj.in" = blogConfig;

    # Git!
    virtualHosts."git.tazj.in" = {
      enableACME = true;
      forceSSL   = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:3000";
      };
    };

    # oslo.pub redirect
    virtualHosts."oslo.pub" = {
      enableACME = true;
      forceSSL   = true;
      extraConfig = "return 302 https://www.google.com/maps/d/viewer?mid=1pJIYY9cuEdt9DuMTbb4etBVq7hs;";
    };

    # Gemma demo instance!
    virtualHosts."gemma.tazj.in" = {
      enableACME = true;
      forceSSL   = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:4242";
      };
    };
  };
}
