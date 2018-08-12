//! This crate contains functionality for dealing with EDN at runtime

#![cfg_attr(feature = "cargo-clippy", warn(clippy))]
#![feature(rust_2018_preview)]
#![warn(rust_2018_idioms)]

pub mod reader;
pub mod writer;
