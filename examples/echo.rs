extern crate breadboard;
extern crate env_logger;
extern crate hyper;
extern crate serde_json;

use breadboard::Breadboard;
use hyper::rt::Future;
use hyper::server::Server;
use hyper::{Request, Response};
use serde_json::Value;
use std::string::ParseError; // waiting for `!`

fn echo(request: Request<Value>) -> Result<Response<Value>, ParseError> {
    Ok(Response::new(request.into_body()))
}

fn main() {
    env_logger::init();

    let board = Breadboard::new().post("/", echo);
    let server = Server::bind(&"127.0.0.1:3000".parse().unwrap())
        .serve(board)
        .map_err(|e| eprintln!("server error: {}", e));
    eprintln!("Listening on http://127.0.0.1:3000/");
    hyper::rt::run(server);
}
