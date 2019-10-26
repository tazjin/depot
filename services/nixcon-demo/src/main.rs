use rouille::Response;
use std::env;
use std::io;
use std::process;

const GREETING: &str = "Haló NixCon!";

fn main() {
    if let Some(arg) = env::args().last() {
        if arg == "--cli" {
            println!("{}", GREETING);
            process::exit(0);
        }
    }

    rouille::start_server("0.0.0.0:8080", move |req| {
        rouille::log(req, io::stdout(), || Response::text(GREETING))
    })
}
