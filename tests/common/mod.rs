use std::fs::File;
use std::io::Read;
use std::process::{Command, Stdio};

pub fn run_ijconsole(test_jfile: &str) -> (String, String) {
    let test_file_path = format!("jlang_programs/{}", test_jfile);
    let mut file = File::open(&test_file_path[..]).expect("file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("size");
    println!("=== TEST FILE CONTENTS ====");
    println!("{}", contents);

    let j_output = Command::new("/usr/bin/ijconsole")
        .stdin(Stdio::from(File::open(&test_file_path[..]).expect("file")))
        .output()
        .expect("failed to execute ijconsole");
    let j_stdout = std::str::from_utf8(&j_output.stdout).unwrap().to_owned();
    let j_stderr = std::str::from_utf8(&j_output.stderr).unwrap().to_owned();
    return (j_stdout, j_stderr);
}

pub fn test(test_file: &str, compile_func: &dyn Fn(&str) -> (String, String)) {
    let (c_stdout, c_stderr) = compile_func(test_file);
    println!("=== BINARY OUTPUT ====");
    println!("{}", c_stdout);
    let (j_stdout, j_stderr) = run_ijconsole(test_file);
    println!("=== J INTERP OUTPUT ====");
    println!("{}", j_stdout);
    assert_eq!(c_stdout, j_stdout);
    assert_eq!(c_stderr, j_stderr);
}
