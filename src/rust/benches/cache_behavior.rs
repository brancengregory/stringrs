use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use rayon::prelude::*;
use regex::Regex as RegexCrate;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

mod common;
use common::*;

/// Benchmark cache hit performance
fn bench_cache_hits(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_hits");

    let patterns = generate_test_patterns(50);
    let mut cache: HashMap<String, RegexCrate> = HashMap::new();

    // Warm up cache
    for pattern in &patterns {
        let re = RegexCrate::new(pattern).unwrap();
        cache.insert(pattern.clone(), re);
    }

    group.throughput(Throughput::Elements(50));
    group.bench_function("cache_hit_retrieval", |b| {
        b.iter(|| {
            for pattern in &patterns {
                if let Some(re) = cache.get(pattern) {
                    black_box(re);
                }
            }
        });
    });

    group.finish();
}

/// Benchmark cache insertion overhead
fn bench_cache_insertion(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_insertion");

    let patterns = generate_test_patterns(100);

    group.bench_function("single_insertion", |b| {
        b.iter_with_setup(
            || {
                let pattern = patterns[0].clone();
                (HashMap::new(), pattern)
            },
            |(mut cache, pattern)| {
                let re = RegexCrate::new(&pattern).unwrap();
                cache.insert(pattern, re);
                black_box(cache);
            },
        );
    });

    group.bench_function("batch_insertion", |b| {
        b.iter_with_setup(
            || patterns.clone(),
            |patterns| {
                let mut cache: HashMap<String, RegexCrate> = HashMap::new();
                for pattern in patterns {
                    let re = RegexCrate::new(&pattern).unwrap();
                    cache.insert(pattern, re);
                }
                black_box(cache);
            },
        );
    });

    group.finish();
}

/// Benchmark thread-safe cache access
fn bench_thread_safe_cache(c: &mut Criterion) {
    let mut group = c.benchmark_group("thread_safe_cache");

    let patterns = generate_test_patterns(50);
    let cache: Arc<Mutex<HashMap<String, RegexCrate>>> = Arc::new(Mutex::new(HashMap::new()));

    // Warm up cache
    {
        let mut cache_guard = cache.lock().unwrap();
        for pattern in &patterns {
            let re = RegexCrate::new(pattern).unwrap();
            cache_guard.insert(pattern.clone(), re);
        }
    }

    for n_threads in [1, 2, 4, 8].iter() {
        group.bench_with_input(
            BenchmarkId::new("concurrent_reads", n_threads),
            n_threads,
            |b, &n_threads| {
                b.iter(|| {
                    (0..1000)
                        .into_par_iter()
                        .with_max_len(1000 / n_threads)
                        .for_each(|i| {
                            let pattern = &patterns[i % patterns.len()];
                            let cache_guard = cache.lock().unwrap();
                            if let Some(re) = cache_guard.get(pattern) {
                                black_box(re);
                            }
                        });
                });
            },
        );
    }

    group.finish();
}

/// Benchmark cache vs no-cache performance
fn bench_cache_vs_no_cache(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_vs_no_cache");

    let n_patterns = 10;
    let patterns = generate_test_patterns(n_patterns);
    let strings = generate_test_strings(1000, 100);

    // With cache
    let mut cache: HashMap<String, RegexCrate> = HashMap::new();
    for pattern in &patterns {
        cache.insert(pattern.clone(), RegexCrate::new(pattern).unwrap());
    }

    group.bench_function("with_cache", |b| {
        b.iter(|| {
            for s in &strings {
                for (pattern, re) in &cache {
                    let _is_match = re.is_match(s);
                    black_box(pattern);
                }
            }
        });
    });

    // Without cache (recompile each time - unrealistic but shows cache value)
    group.bench_function("no_cache", |b| {
        b.iter(|| {
            for s in &strings {
                for pattern in &patterns {
                    let re = RegexCrate::new(pattern).unwrap();
                    let _is_match = re.is_match(s);
                }
            }
        });
    });

    group.finish();
}

/// Benchmark memory overhead of cache
fn bench_cache_memory_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache_memory_overhead");

    for n_patterns in [10, 50, 100, 500].iter() {
        let patterns = generate_test_patterns(*n_patterns);

        group.bench_with_input(
            BenchmarkId::new("memory_per_pattern", n_patterns),
            &patterns,
            |b, patterns| {
                b.iter(|| {
                    let mut cache: HashMap<String, RegexCrate> = HashMap::new();
                    for pattern in patterns {
                        let re = RegexCrate::new(pattern).unwrap();
                        cache.insert(pattern.clone(), re);
                    }
                    black_box(cache.len());
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_cache_hits,
    bench_cache_insertion,
    bench_thread_safe_cache,
    bench_cache_vs_no_cache,
    bench_cache_memory_overhead
);
criterion_main!(benches);
