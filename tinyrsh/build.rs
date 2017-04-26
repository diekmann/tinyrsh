use std::process::Command;
use std::env;
use std::path::Path;
use std::io::{self, Write};


//http://doc.crates.io/build-script.html

fn main() {
	io::stderr().write(b"running build.rs\n");
    let out_dir = env::var("OUT_DIR").unwrap();

    let s = Command::new("gcc").args(&["-Wall", "-Wextra", "-Werror", "-fno-strict-aliasing", "-c", "cpp_defs.c", "-fPIC", "-o"])
                       .arg(&format!("{}/cpp_defs.o", out_dir))
                       .status().unwrap();
	if !s.success(){
		std::process::exit(s.code().unwrap());
	}
    let s = Command::new("ar").args(&["crus", "libcppdefs.a", "cpp_defs.o"])
                      .current_dir(&Path::new(&out_dir))
                      .status().unwrap();
	if !s.success(){
		std::process::exit(s.code().unwrap());
	}

    println!("cargo:rustc-link-search=native={}", out_dir);
    println!("cargo:rustc-link-lib=static=cppdefs");
}
