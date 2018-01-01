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
  addSSL = true;
  locations."/" = {
    proxyPass = "http://127.0.0.1:8000";
  };
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

  # Set up reverse proxy
  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;

    virtualHosts."tazj.in" = blogConfig;
    virtualHosts."www.tazj.in" = blogConfig;
  };
}
