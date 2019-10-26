use std::io;
use rouille::Response;

fn main() {
    rouille::start_server("0.0.0.0:8080", move |req| {
        rouille::log(req, io::stdout(), || Response::text("Haló NixCon!"))
    })
}
