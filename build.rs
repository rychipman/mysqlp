extern crate prost_build;

fn main() {
    let protos = vec!["ast.proto"];
    prost_build::compile_protos(&protos,
                                &["proto/"]).unwrap();
}