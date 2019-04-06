extern crate criterion;
extern crate tempfile;

use criterion::{criterion_group, criterion_main, Benchmark, Criterion};
use tempfile::NamedTempFile;

fn chapter_one_unoptimized_benchmark(c: &mut Criterion) {
    let ijsfile = "ctest_ch1_learningjbook.ijs";
    c.bench(
        ijsfile,
        Benchmark::new("unoptimized", move |b| {
            b.iter(|| {
                let outpath = String::from(
                    NamedTempFile::new()
                        .unwrap()
                        .path()
                        .to_str()
                        .expect("valid tempfile path"),
                );
                jcompilerlib::compile(
                    &format!("jlang_programs/{}", ijsfile)[..],
                    None,
                    0,
                    false,
                    false,
                    false,
                    Some(outpath),
                )
            })
        })
        .with_function("optimized", move |b| {
            b.iter(|| {
                let outpath = String::from(
                    NamedTempFile::new()
                        .unwrap()
                        .path()
                        .to_str()
                        .expect("valid tempfile path"),
                );
                jcompilerlib::compile(
                    &format!("jlang_programs/{}", ijsfile)[..],
                    None,
                    3,
                    true,
                    false,
                    false,
                    Some(outpath),
                )
            })
        })
        .sample_size(20),
    );
}

criterion_group!(benches, chapter_one_unoptimized_benchmark);
criterion_main!(benches);
