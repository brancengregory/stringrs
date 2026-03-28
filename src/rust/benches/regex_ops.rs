use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use fancy_regex::Regex as FancyRegex;
use regex::Regex as RegexCrate;
use std::time::Duration;

mod common;
use common::*;

/// Benchmark regex compilation (cached vs uncached)
fn bench_regex_compilation(c: &mut Criterion) {
    let mut group = c.benchmark_group("regex_compilation");

    let patterns = generate_test_patterns(10);

    // Benchmark: Uncached compilation
    group.bench_function("uncached", |b| {
        b.iter(|| {
            for pattern in &patterns {
                let _re = RegexCrate::new(pattern).unwrap();
                black_box(&pattern);
            }
        });
    });

    // Benchmark: First-time cache insert
    group.bench_function("cache_cold", |b| {
        b.iter(|| {
            // Note: This is testing the overhead of cache insertion
            // In real use, this would only happen once per pattern
            let mut cache: std::collections::HashMap<String, RegexCrate> =
                std::collections::HashMap::new();
            for pattern in &patterns {
                let re = RegexCrate::new(pattern).unwrap();
                cache.insert(pattern.clone(), re);
            }
            black_box(cache);
        });
    });

    group.finish();
}

/// Benchmark single pattern matching at different scales
fn bench_single_pattern_matching(c: &mut Criterion) {
    let mut group = c.benchmark_group("single_pattern_matching");
    group.measurement_time(Duration::from_secs(10));

    let pattern = "[a-z]+[0-9]{2}";
    let compiled = RegexCrate::new(pattern).unwrap();

    for (n_strings, _) in BENCH_SIZES.iter().take(4) {
        let strings = generate_test_strings(*n_strings, 100);

        group.throughput(Throughput::Elements(*n_strings as u64));
        group.bench_with_input(
            BenchmarkId::new("standard_regex", n_strings),
            &strings,
            |b, strings| {
                b.iter(|| {
                    let results: Vec<bool> = strings.iter().map(|s| compiled.is_match(s)).collect();
                    black_box(results);
                });
            },
        );
    }

    group.finish();
}

/// Benchmark fancy-regex vs standard regex
fn bench_fancy_vs_standard(c: &mut Criterion) {
    let mut group = c.benchmark_group("fancy_vs_standard");
    group.measurement_time(Duration::from_secs(10));

    // Standard pattern
    let standard_pattern = r"\b\w{5,}\b";
    // Fancy pattern (backreference)
    let fancy_pattern = r"(.)\1";

    let standard_re = RegexCrate::new(standard_pattern).unwrap();
    let fancy_re = FancyRegex::new(fancy_pattern).unwrap();

    let strings = generate_test_strings(10_000, 100);

    group.throughput(Throughput::Elements(10_000));

    group.bench_function("standard_regex", |b| {
        b.iter(|| {
            let results: Vec<bool> = strings.iter().map(|s| standard_re.is_match(s)).collect();
            black_box(results);
        });
    });

    group.bench_function("fancy_regex_backref", |b| {
        b.iter(|| {
            let results: Vec<bool> = strings
                .iter()
                .map(|s| fancy_re.is_match(s).unwrap_or(false))
                .collect();
            black_box(results);
        });
    });

    group.finish();
}

/// Benchmark multi-pattern matching with flat array output
fn bench_multi_pattern_matching(c: &mut Criterion) {
    let mut group = c.benchmark_group("multi_pattern_matching");
    group.measurement_time(Duration::from_secs(10));

    for (n_strings, n_patterns) in BENCH_SIZES.iter().take(4) {
        let strings = generate_test_strings(*n_strings, 100);
        let patterns = generate_test_patterns(*n_patterns);
        let regexes = precompile_regexes(&patterns);

        let total_matches = n_strings * n_patterns;

        group.throughput(Throughput::Elements(total_matches as u64));
        group.bench_with_input(
            BenchmarkId::new("flat_array_output", format!("{}x{}", n_strings, n_patterns)),
            &(strings, regexes),
            |b, (strings, regexes)| {
                b.iter(|| {
                    let mut results: Vec<bool> = Vec::with_capacity(total_matches);
                    for s in strings {
                        for re in regexes {
                            results.push(re.is_match(s));
                        }
                    }
                    black_box(results);
                });
            },
        );
    }

    group.finish();
}

/// Benchmark match density effects
fn bench_match_density(c: &mut Criterion) {
    let mut group = c.benchmark_group("match_density_effects");

    let pattern = "[aeiou]{2}";
    let n_strings = 10_000;

    for density in [0.01, 0.1, 0.5, 0.9].iter() {
        let strings = generate_strings_with_matches(n_strings, 100, pattern, *density);
        let compiled = RegexCrate::new(pattern).unwrap();

        group.bench_with_input(
            BenchmarkId::new("density", format!("{:.0}%", density * 100.0)),
            &strings,
            |b, strings| {
                b.iter(|| {
                    let count = strings.iter().filter(|s| compiled.is_match(s)).count();
                    black_box(count);
                });
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_regex_compilation,
    bench_single_pattern_matching,
    bench_fancy_vs_standard,
    bench_multi_pattern_matching,
    bench_match_density
);
criterion_main!(benches);
