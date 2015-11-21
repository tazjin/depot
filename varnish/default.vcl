vcl 4.0;

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
}

sub vcl_synth {
        # Execute redirects
        if (resp.status == 301) {
                set resp.http.Location = req.url;
                return (deliver);
        }
}
