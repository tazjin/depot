vcl 4.0;
import std;

# By default, Varnish will run on the same servers as the blog. Inside of
# Kubernetes this will be inside the same pod.

backend default {
        .host = "localhost";
        .port = "8000";
}

# Purge requests should be accepted from localhost
acl purge {
        "localhost";
}

sub vcl_recv {
        # Allow HTTP PURGE from ACL above
        if (req.method == "PURGE" && client.ip ~ purge) {
                return (purge);
        }

        # Don't cache admin page
        if (req.url ~ "^/admin") {
                return (pass);
        }

        # Redirect non-www to www and non-HTTPS to HTTPS
        if (req.http.host ~ "^tazj.in" || std.port(local.ip) == 6081) {
                return (synth (750, ""));
        }
}

sub vcl_backend_response {
        # Cache everything for at least 1 minute.
        if (beresp.ttl < 1m) {
                set beresp.ttl = 1m;
        }
}

sub vcl_deliver {
        # Add an HSTS header to everything
        set resp.http.Strict-Transport-Security = "max-age=31536000;includeSubdomains;preload";

        if (obj.hits > 0) {
                set resp.http.X-Cache = "HIT";
        } else {
                set resp.http.X-Cache = "MISS";
        }
}

sub vcl_synth {
        # Execute TLS or www. redirect
        if (resp.status == 750) {
                set resp.http.Location = "https://www.tazj.in" + req.url;
                set resp.http.Strict-Transport-Security = "max-age=31536000;includeSubdomains;preload";
                set resp.status = 301;
                return (deliver);
        }
}
