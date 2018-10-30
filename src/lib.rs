//! Breadboard is a simple JSON API request router for [hyper].
//!
//! Like [electronics breadboards], Breadboard is well-suited to prototyping your API. Also like
//! electronics breadboards, its functionality is limited and if you bend it too hard it will snap.
//!
//! To create a hyper [`Service`], create a `Breadboard` with [`Breadboard::new`], use methods like
//! [`Breadboard::get`] to add handlers, then use it in [`hyper::server::Builder::serve`].
//!
//! [hyper]: ../hyper/index.html
//! [electronics breadboards]: https://en.wikipedia.org/wiki/Breadboard
//! [`Service`]: ../hyper/service/trait.Service.html
//! [`Breadboard::new`]: struct.Breadboard.html#method.new
//! [`Breadboard::get`]: struct.Breadboard.html#method.get
//! [`hyper::server::Builder::serve`]: ../hyper/server/struct.Builder.html#method.serve
//!
//! ## Quick start
//!
//! ```
//! extern crate breadboard;
//! extern crate hyper;
//!
//! use breadboard::Breadboard;
//! use hyper::server::Server;
//! use hyper::{Request, Response};
//! use std::string::ParseError; // waiting for `!`
//!
//! fn hello(_: Request<()>) -> Result<Response<&'static str>, ParseError> {
//!     Ok(Response::new("Hello, world!"))
//! }
//!
//! let board = Breadboard::new().get("/", hello);
//! let server = Server::bind(&"127.0.0.1:3000".parse().unwrap())
//!     .serve(board);
//! // hyper::rt::run(server.map_err(|e| eprintln!("server error: {}", e)));
//! ```
//!
//! You can also use closures as handlers:
//!
//! ```
//! # extern crate breadboard;
//! # extern crate hyper;
//! # use breadboard::Breadboard;
//! # use hyper::server::Server;
//! # use hyper::{Request, Response};
//! # use std::string::ParseError; // waiting for `!`
//! let board = Breadboard::new().get(
//!     "/",
//!     |_: Request<()>| -> Result<Response<&'static str>, ParseError> {
//!         Ok(Response::new("Hello, world!"))
//!     },
//! );
//! let server = Server::bind(&"127.0.0.1:3000".parse().unwrap())
//!     .serve(board);
//! ```
//!
//! ## Handlers that return Futures
//!
//! Handlers return a type that implement [`IntoFuture`]. This includes `Result`, as well as all
//! types that implement [`Future`].
//!
//! This example handler makes an HTTP request and responds with the response it receives. It
//! returns a `Future` that resolves after the request completes.
//!
//! ```
//! extern crate breadboard;
//! extern crate hyper;
//!
//! use breadboard::Breadboard;
//! use hyper::client::Client;
//! use hyper::rt::{Future, Stream};
//! use hyper::server::Server;
//! use hyper::{Body, Request, Response};
//!
//! fn proxy(_: Request<()>) -> impl Future<Item = Response<Vec<u8>>, Error = hyper::Error> {
//!     let client: Client<_, Body> = Client::builder().build_http();
//!     client
//!         .get("http://icanhazip.com/".parse().unwrap())
//!         .and_then(|response| {
//!             let (parts, body) = response.into_parts();
//!             body.concat2()
//!                 .map(|chunk| Response::from_parts(parts, chunk.into_bytes().to_vec()))
//!         })
//! }
//!
//! let board = Breadboard::new().get("/", proxy);
//! let server = Server::bind(&"127.0.0.1:3000".parse().unwrap())
//!     .serve(board);
//! ```
//!
//! [`IntoFuture`]: ../futures/future/trait.IntoFuture.html
//! [`Future`]: ../futures/future/trait.Future.html
//!
//! ## Deserialize and Serialize
//!
//! The real power of Breadboard is that request and response bodies can be anything as long as
//! they implement Deserialize and Serialize, respectively.
//!
//! ```
//! extern crate breadboard;
//! extern crate hyper;
//! extern crate serde;
//! #[macro_use]
//! extern crate serde_derive;
//!
//! use breadboard::Breadboard;
//! use hyper::client::Client;
//! use hyper::rt::{Future, Stream};
//! use hyper::server::Server;
//! use hyper::{Body, Request, Response};
//! use std::string::ParseError; // waiting for `!`
//!
//! #[derive(Debug, Deserialize)]
//! struct Message {
//!     message: String,
//! }
//!
//! fn message(request: Request<Message>) -> Result<Response<String>, ParseError> {
//!     Ok(Response::new(request.into_body().message))
//! }
//!
//! # fn main() {
//! let board = Breadboard::new().post("/", message);
//! let server = Server::bind(&"127.0.0.1:3000".parse().unwrap())
//!     .serve(board);
//! # }
//! ```

#![warn(
    future_incompatible,
    rust_2018_compatibility,
    rust_2018_idioms,
)]
#![deny(
    missing_docs,
    unused,
    unused_extern_crates,
    missing_copy_implementations,
    missing_debug_implementations
)]
#![cfg_attr(feature = "cargo-clippy", warn(clippy_pedantic))]

extern crate futures;
extern crate hyper;
extern crate regex;
extern crate serde;
extern crate serde_json;

use futures::future::{Future, FutureResult, IntoFuture};
use futures::Stream;
use hyper::header::{HeaderValue, CONTENT_TYPE};
use hyper::{Body, Method, Request, Response};
use regex::RegexSet;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::sync::Arc;

macro_rules! status {
    ($code:expr) => {
        Response::builder()
            .status($code)
            .body(Body::empty())
            .expect("cannot fail")
    };
}

mod error;

pub use crate::error::{BuildError, RequestError};

type HandlerFuture = Box<dyn Future<Item = Response<Vec<u8>>, Error = RequestError> + Send>;
type Handler = Arc<dyn Fn(Request<&[u8]>) -> HandlerFuture + Send + Sync>;

fn convert_route(route: &str) -> Result<String, BuildError> {
    Ok(if route.starts_with('/') {
        format!("^{}$", regex::escape(route))
    } else {
        format!("^/{}$", regex::escape(route))
    })
}

// If the body is empty, serde_json will throw an EOF. Appease it by trying to deserialize `null`.
fn from_slice<T>(buf: &[u8]) -> Result<T, RequestError>
where
    for<'de> T: Deserialize<'de>,
{
    serde_json::from_slice(if buf.is_empty() { b"null" } else { buf }).map_err(RequestError::De)
}

struct RouteList<'a>(&'a Vec<(String, Handler)>);

impl<'a> Debug for RouteList<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries(self.0.iter().map(|(s, _)| s))
            .finish()
    }
}

/// A router builder.
///
/// Register your routes, then use hyper's [`NewService`] trait to create the service.
///
/// See the crate-level documentation for examples.
///
/// ## Anatomy of a handler
///
/// A handler is a function that takes a single argument, [`Request<T>`]. `T` must implement
/// [`serde::Deserialize`].
///
/// The handler returns a type that implements [`IntoFuture`]. The future's [`Item`] is a
/// [`Response<T>`], where `T` must implement [`serde::Serialize`]. The future's
/// [`Error`][future-error] can be anything that implements [`std::error::Error`], [`Send`], and
/// [`Sync`].
///
/// If you aren't doing any work in your handler that requires use of futures, you can simply
/// return a [`Result`], as `Result` implements `IntoFuture`.
///
/// [`NewService`]: ../hyper/service/trait.NewService.html
/// [`Request<T>`]: ../hyper/struct.Request.html
/// [`serde::Deserialize`]: ../serde/trait.Deserialize.html
/// [`IntoFuture`]: ../futures/future/trait.IntoFuture.html
/// [`Item`]: ../futures/future/trait.Future.html#associatedtype.Item
/// [`Response<T>`]: ../hyper/struct.Response.html
/// [`serde::Serialize`]: ../serde/trait.Serialize.html
/// [future-error]: ../futures/future/trait.Future.html#associatedtype.Error
/// [`std::error::Error`]: https://doc.rust-lang.org/nightly/std/error/trait.Error.html
/// [`Send`]: https://doc.rust-lang.org/nightly/std/marker/trait.Send.html
/// [`Sync`]: https://doc.rust-lang.org/nightly/std/marker/trait.Sync.html
/// [`Result`]: https://doc.rust-lang.org/nightly/std/result/enum.Result.html
/// [`String`]: https://doc.rust-lang.org/nightly/std/string/struct.String.html
/// [`!`]: https://doc.rust-lang.org/nightly/std/primitive.never.html
#[derive(Default)]
pub struct Breadboard {
    routes: HashMap<Method, Vec<(String, Handler)>>,
}

impl Debug for Breadboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.routes.iter().map(|(method, v)| (method, RouteList(v))))
            .finish()
    }
}

macro_rules! method_shortcut {
    ($lower:ident, $upper:ident, $upper_str:expr) => (
        #[doc = "Add a handler for "]
        #[doc = $upper_str]
        #[doc = " requests for a route."]
        pub fn $lower<A, F, R, D, S, E>(self, route: A, handler: F) -> Self
        where
            A: AsRef<str>,
            F: Fn(Request<D>) -> R + Send + Sync + 'static,
            R: IntoFuture<Item = Response<S>, Error = E> + 'static,
            R::Future: Send,
            for<'de> D: Deserialize<'de> + Send + 'static,
            S: Serialize + 'static,
            E: Into<Box<dyn std::error::Error + Send + Sync>>,
        {
            self.route(Method::$upper, route, handler)
        }
    );

    ($lower:ident, $upper:tt) => (
        method_shortcut!($lower, $upper, stringify!($upper));
    );
}

impl Breadboard {
    /// Start a new `Breadboard`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a handler for a route with the given method.
    pub fn route<A, F, R, D, S, E>(self, method: Method, route: A, handler: F) -> Self
    where
        A: AsRef<str>,
        F: Fn(Request<D>) -> R + Send + Sync + 'static,
        R: IntoFuture<Item = Response<S>, Error = E> + 'static,
        R::Future: Send,
        for<'de> D: Deserialize<'de> + Send + 'static,
        S: Serialize + 'static,
        E: Into<Box<dyn std::error::Error + Send + Sync>>,
    {
        let mut s = self;
        s.routes.entry(method).or_insert_with(Vec::new).push((
            route.as_ref().to_owned(),
            Arc::new(move |request| {
                let (parts, body) = request.into_parts();
                let request = match from_slice(body) {
                    Ok(buf) => Request::from_parts(parts, buf),
                    Err(err) => return Box::new(FutureResult::from(Err(err))),
                };
                Box::new(
                    handler(request)
                        .into_future()
                        .map_err(|err| RequestError::Handler(err.into()))
                        .and_then(|response| {
                            let (res_parts, res_body) = response.into_parts();
                            let mut response = Response::from_parts(
                                res_parts,
                                serde_json::to_vec(&res_body).map_err(RequestError::Ser)?,
                            );
                            response
                                .headers_mut()
                                .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
                            Ok(response)
                        }),
                )
            }),
        ));
        s
    }

    method_shortcut!(get, GET);
    method_shortcut!(post, POST);
    method_shortcut!(put, PUT);
    method_shortcut!(delete, DELETE);
    method_shortcut!(head, HEAD);
    method_shortcut!(options, OPTIONS);
    method_shortcut!(connect, CONNECT);
    method_shortcut!(patch, PATCH);
    method_shortcut!(trace, TRACE);

    /// fixme
    pub fn build(&self) -> Result<Service, BuildError> {
        Ok(Service {
            routes: self
                .routes
                .iter()
                .map(|(method, v)| {
                    let set = RegexSet::new(
                        v.iter()
                            .map(|x| convert_route(&x.0))
                            .collect::<Result<Vec<_>, _>>()?,
                    ).expect("breadboard should not generate invalid regexes");
                    Ok((
                        method.clone(),
                        (set, v.iter().map(|x| x.1.clone()).collect()),
                    ))
                }).collect::<Result<HashMap<_, _>, _>>()?,
        })
    }
}

impl hyper::service::NewService for Breadboard {
    type ReqBody = Body;
    type ResBody = Body;
    type Error = RequestError;
    type Service = Service;
    type Future = FutureResult<Service, BuildError>;
    type InitError = BuildError;

    fn new_service(&self) -> Self::Future {
        self.build().into()
    }
}

struct ServiceRoutes<'a>(&'a HashMap<Method, (RegexSet, Vec<Handler>)>);

impl<'a> Debug for ServiceRoutes<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.0.iter().map(|(method, v)| (method, &v.0)))
            .finish()
    }
}

/// The hyper [`Service`].
///
/// [`Service`]: ../hyper/service/trait.Service.html
pub struct Service {
    routes: HashMap<Method, (RegexSet, Vec<Handler>)>,
}

impl Debug for Service {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Service")
            .field("routes", &ServiceRoutes(&self.routes))
            .finish()
    }
}

impl hyper::service::Service for Service {
    type ReqBody = Body;
    type ResBody = Body;
    type Error = RequestError;
    type Future = Box<dyn Future<Item = Response<Body>, Error = RequestError> + Send>;

    fn call(&mut self, request: Request<Self::ReqBody>) -> Self::Future {
        let handler = match self.routes.get(request.method()).and_then(|(set, v)| {
            set.matches(request.uri().path())
                .into_iter()
                .next()
                .and_then(|i| v.get(i))
        }) {
            Some(h) => h.clone(),
            None => return Box::new(FutureResult::from(Ok(status!(404)))),
        };
        let (parts, body) = request.into_parts();
        Box::new(
            body.concat2()
                .map_err(RequestError::Hyper)
                .and_then(move |chunk| handler(Request::from_parts(parts, &chunk)))
                .map(|response| response.map(Body::from)),
        )
    }
}
