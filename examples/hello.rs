extern crate breadboard;
extern crate env_logger;
extern crate hyper;

use breadboard::Breadboard;
use hyper::rt::Future;
use hyper::server::Server;
use hyper::{Request, Response};
use std::string::ParseError; // waiting for `!`

fn hello(_: Request<()>) -> Result<Response<&'static str>, ParseError> {
    Ok(Response::new("Hello, world!"))
}

fn main() {
    env_logger::init();

    let board = Breadboard::new().get("/", hello);
    let server = Server::bind(&"127.0.0.1:3000".parse().unwrap())
        .serve(board)
        .map_err(|e| eprintln!("server error: {}", e));
    eprintln!("Listening on http://127.0.0.1:3000/");
    hyper::rt::run(server);
}
