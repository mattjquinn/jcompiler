#[no_mangle]
pub extern "C" fn hello() {
    println!("Hello from the J runtime.");
}