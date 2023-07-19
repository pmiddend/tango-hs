use std::process;

fn main() {
    if std::env::var("DOCS_RS").is_ok() {
        return;
    }
    let tango_lib = match pkg_config::probe_library("tango") {
        Ok(lib) => lib,
        Err(err) => { print!("{}---", err); process::exit(1); }
    };
    let mut config = cc::Build::new();
    config.cpp(true);
    config.flag("-std=c++14");
    config.flag("-Wno-deprecated");
    config.file("src/c_tango_proxy.c");
    config.file("src/c_tango_command.c");
    config.file("src/c_tango_attribute.c");
    config.file("src/c_tango_dbase.c");
    config.include("src");
    for path in tango_lib.include_paths {
        config.include(&path);
        // Tango 9.4+: only /usr/include in pkg-config
        config.include(path.join("tango"));
    }
    config.compile("libc_tango.a");
}
