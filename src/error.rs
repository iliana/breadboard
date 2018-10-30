#![cfg_attr(feature = "cargo-clippy", allow(stutter))]

use std;
use std::fmt::{self, Display};

/// An error when creating the [`Service`] from a [`Breadboard`].
///
/// [`Service`]: ../hyper/service/trait.Service.html
/// [`Breadboard`]: struct.Breadboard.html
#[allow(missing_copy_implementations)]
#[cfg_attr(feature = "cargo-clippy", allow(empty_enum))]
#[derive(Debug)]
pub enum BuildError {}

impl Display for BuildError {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

impl std::error::Error for BuildError {}

/// An error during request handling.
#[derive(Debug)]
pub enum RequestError {
    /// An error returned by the handler function.
    Handler(Box<dyn std::error::Error + Send + Sync>),
    /// An error that occurred during handling of the HTTP stream.
    Hyper(hyper::Error),
    /// An error that occurred when deserializing the request body.
    De(serde_json::Error),
    /// An error that occurred when serializing the response body.
    Ser(serde_json::Error),
}

impl Display for RequestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RequestError::Handler(e) => e.fmt(f),
            RequestError::Hyper(e) => e.fmt(f),
            RequestError::De(e) | RequestError::Ser(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for RequestError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            RequestError::Handler(_) => None,
            RequestError::Hyper(e) => Some(e),
            RequestError::De(e) | RequestError::Ser(e) => Some(e),
        }
    }
}
