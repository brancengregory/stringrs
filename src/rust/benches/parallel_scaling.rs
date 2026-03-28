use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use rayon::prelude::*;
use regex::Regex as RegexCrate;
use std::cell::RefCell;
use std::sync::Arc;

mod common;
use common::*;

/// Benchmark string-parallel scaling with different thread counts
fn bench_string_parallel_scaling(c: &mut Criterion) {
    let mut group = c.benchmark_group("string_parallel_scaling");

    let n_strings = 50_000;
    let n_patterns = 10;
    let strings = Arc::new(generate_test_strings(n_strings, 100));
    let patterns = Arc::new(generate_test_patterns(n_patterns));
    let regexes = Arc::new(
        patterns
            .iter()
            .map(|p| RegexCrate::new(p).unwrap())
            .collect::<Vec<_>>(),
    );

    for n_threads in THREAD_COUNTS.iter() {
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(*n_threads)
            .build()
            .unwrap();

        group.throughput(Throughput::Elements(n_strings as u64));
        group.bench_with_input(
            BenchmarkId::new("threads", n_threads),
            n_threads,
            |b, _| {
                b.iter(|| {
                    pool.install(|| {
                        let strings = strings.clone();
                        let regexes = regexes.clone();

                        let results: Vec<Vec<bool>> = strings
                            .par_chunks(5000)
                            .map(|chunk| {
                                // Thread-local regex compilation
                                thread_local! {
                                    static LOCAL_RES: RefCell<Vec<RegexCrate>> = RefCell::new(Vec::new());
                                }

                                LOCAL_RES.with(|local_res| {
                                    let mut local_regexes = local_res.borrow_mut();
                                    if local_regexes.is_empty() {
                                        *local_regexes = (*regexes).clone();
                                    }

                                    chunk
                                        .iter()
                                        .map(|s| {
                                            local_regexes.iter().any(|re| re.is_match(s))
                                        })
                                        .collect()
                                })
                            })
                            .collect();

                        black_box(results);
                    });
                });
            },
        );
    }

    group.finish();
}

/// Benchmark pattern-parallel scaling
fn bench_pattern_parallel_scaling(c: &mut Criterion) {
    let mut group = c.benchmark_group("pattern_parallel_scaling");

    let n_strings = 10_000;
    let n_patterns = 50;
    let strings = Arc::new(generate_test_strings(n_strings, 100));
    let patterns = Arc::new(generate_test_patterns(n_patterns));

    for n_threads in THREAD_COUNTS.iter() {
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(*n_threads)
            .build()
            .unwrap();

        group.throughput(Throughput::Elements((n_strings * n_patterns) as u64));
        group.bench_with_input(BenchmarkId::new("threads", n_threads), n_threads, |b, _| {
            b.iter(|| {
                pool.install(|| {
                    let strings = strings.clone();
                    let patterns = patterns.clone();

                    let results: Vec<Vec<bool>> = patterns
                        .par_iter()
                        .map(|pattern| {
                            let re = RegexCrate::new(pattern).unwrap();
                            strings.iter().map(|s| re.is_match(s)).collect()
                        })
                        .collect();

                    black_box(results);
                });
            });
        });
    }

    group.finish();
}

/// Benchmark chunk size effects
fn bench_chunk_size_effects(c: &mut Criterion) {
    let mut group = c.benchmark_group("chunk_size_effects");

    let n_strings = 100_000;
    let n_patterns = 10;
    let strings = Arc::new(generate_test_strings(n_strings, 100));
    let regexes = Arc::new(
        generate_test_patterns(n_patterns)
            .iter()
            .map(|p| RegexCrate::new(p).unwrap())
            .collect::<Vec<_>>(),
    );

    for chunk_size in CHUNK_SIZES.iter() {
        group.throughput(Throughput::Elements(n_strings as u64));
        group.bench_with_input(
            BenchmarkId::new("size", chunk_size),
            chunk_size,
            |b, &chunk_size| {
                b.iter(|| {
                    let strings = strings.clone();
                    let regexes = regexes.clone();

                    let results: Vec<Vec<bool>> = (0..n_strings)
                        .collect::<Vec<_>>()
                        .par_chunks(chunk_size)
                        .map(|chunk| {
                            let local_regexes = regexes.clone();
                            chunk
                                .iter()
                                .map(|&idx| {
                                    let s = &strings[idx];
                                    local_regexes.iter().map(|re| re.is_match(s)).collect()
                                })
                                .collect()
                        })
                        .collect();

                    black_box(results);
                });
            },
        );
    }

    group.finish();
}

/// Benchmark parallel overhead vs sequential
fn bench_parallel_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("parallel_overhead");

    let pattern = "[a-z]+[0-9]{2}";
    let compiled = RegexCrate::new(pattern).unwrap();

    for (n_strings, _) in BENCH_SIZES.iter().take(3) {
        let strings = generate_test_strings(*n_strings, 100);

        // Sequential baseline
        group.bench_with_input(
            BenchmarkId::new("sequential", n_strings),
            &strings,
            |b, strings| {
                b.iter(|| {
                    let results: Vec<bool> = strings.iter().map(|s| compiled.is_match(s)).collect();
                    black_box(results);
                });
            },
        );

        // Parallel (if strings > 100)
        if *n_strings >= 100 {
            group.bench_with_input(
                BenchmarkId::new("parallel", n_strings),
                &strings,
                |b, strings| {
                    b.iter(|| {
                        let results: Vec<bool> =
                            strings.par_iter().map(|s| compiled.is_match(s)).collect();
                        black_box(results);
                    });
                },
            );
        }
    }

    group.finish();
}

/// Benchmark strategy selection heuristics
fn bench_strategy_selection(c: &mut Criterion) {
    let mut group = c.benchmark_group("strategy_selection");

    // Many strings, few patterns - string-parallel should win
    let many_strings = Arc::new(generate_test_strings(50_000, 100));
    let few_patterns = Arc::new(generate_test_patterns(5));

    group.bench_function("many_strings_few_patterns", |b| {
        b.iter(|| {
            // Simulate string-parallel strategy
            let strings = many_strings.clone();
            let patterns = few_patterns.clone();

            let regexes: Vec<_> = patterns
                .iter()
                .map(|p| RegexCrate::new(p).unwrap())
                .collect();

            let results: Vec<Vec<bool>> = strings
                .par_chunks(5000)
                .map(|chunk| {
                    chunk
                        .iter()
                        .map(|s| regexes.iter().map(|re| re.is_match(s)).collect())
                        .collect()
                })
                .collect();

            black_box(results);
        });
    });

    // Few strings, many patterns - pattern-parallel should win
    let few_strings = Arc::new(generate_test_strings(1_000, 100));
    let many_patterns = Arc::new(generate_test_patterns(100));

    group.bench_function("few_strings_many_patterns", |b| {
        b.iter(|| {
            // Simulate pattern-parallel strategy
            let strings = few_strings.clone();
            let patterns = many_patterns.clone();

            let results: Vec<Vec<bool>> = patterns
                .par_iter()
                .map(|pattern| {
                    let re = RegexCrate::new(pattern).unwrap();
                    strings.iter().map(|s| re.is_match(s)).collect()
                })
                .collect();

            black_box(results);
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_string_parallel_scaling,
    bench_pattern_parallel_scaling,
    bench_chunk_size_effects,
    bench_parallel_overhead,
    bench_strategy_selection
);
criterion_main!(benches);
